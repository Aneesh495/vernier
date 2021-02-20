#lang racket/base

;;; vernier/sdk/racket/tracer.rkt
;;; Idiomatic Racket client SDK for distributed tracing and context propagation.
;;; Provides thread-local dynamic extent tracking via parameters and syntax forms.

(require racket/contract
         racket/list
         racket/string
         racket/tcp
         json
         "../../core/time.rkt"
         "../../core/id.rkt"
         "../../core/span.rkt"
         "../../protocol/w3c.rkt")

(provide
 (contract-out
  ;; Tracer configuration and handle
  [struct tracer ([service-name string?]
                  [collector-host string?]
                  [collector-port exact-positive-integer?]
                  [buffer-queue box?]
                  [flush-semaphore semaphore?])]
  
  [make-tracer (->* (string?)
                    (#:host string?
                     #:port exact-positive-integer?)
                    tracer?)]
  [tracer-flush! (-> tracer? boolean?)]
  [tracer-start-span (->* (tracer? string?)
                          (#:parent-span (or/c span? #f)
                           #:kind symbol?
                           #:attributes hash?)
                          span?)]
  [tracer-finish-span! (->* (tracer? span?)
                            (#:status symbol?
                             #:status-message (or/c string? #f)
                             #:attributes hash?)
                            span?)]
  
  ;; Context parameters
  [current-tracer (parameter/c (or/c tracer? #f))]
  [current-span (parameter/c (or/c span? #f))]
  
  ;; Context propagation helpers
  [inject-w3c-traceparent (-> span? string?)]
  [extract-w3c-parent (-> string? (values (or/c string? #f) (or/c string? #f)))])
 with-span)

(struct tracer (service-name collector-host collector-port buffer-queue flush-semaphore)
  #:transparent)

(define current-tracer (make-parameter #f))
(define current-span (make-parameter #f))

(define (make-tracer service-name
                     #:host [host "127.0.0.1"]
                     #:port [port 9876])
  (tracer service-name host port (box '()) (make-semaphore 1)))

;; Starts a new span under the active tracer
(define (tracer-start-span t name
                           #:parent-span [parent (current-span)]
                           #:kind [kind 'internal]
                           #:attributes [attrs (hash)])
  (define tid
    (if parent
        (span-trace-id parent)
        (make-trace-id)))
  (define pid
    (and parent (span-span-id parent)))
  (define sid (make-span-id))
  (define start-ns (current-time-nanos))
  
  (make-span #:trace-id tid
             #:span-id sid
             #:parent-id pid
             #:name name
             #:service-name (tracer-service-name t)
             #:kind kind
             #:start-time start-ns
             #:end-time start-ns
             #:status-code 'unset
             #:attributes attrs))

;; Finishes span, updates end time and enqueues to tracer buffer
(define (tracer-finish-span! t s
                             #:status [stat 'ok]
                             #:status-message [stat-msg #f]
                             #:attributes [extra-attrs (hash)])
  (define end-ns (current-time-nanos))
  (define dur (- end-ns (span-start-time s)))
  (define merged-attrs
    (for/fold ([acc (span-attributes s)]) ([(k v) (in-hash extra-attrs)])
      (hash-set acc k v)))
  (define completed-span
    (struct-copy span s
                 [end-time end-ns]
                 [duration dur]
                 [status-code stat]
                 [status-message (or stat-msg "")]
                 [attributes merged-attrs]))
  
  (call-with-semaphore
   (tracer-flush-semaphore t)
   (lambda ()
     (set-box! (tracer-buffer-queue t)
               (cons completed-span (unbox (tracer-buffer-queue t))))))
  completed-span)

;; Flushes buffered spans to Vernier collector via HTTP POST /api/v1/spans
(define (tracer-flush! t)
  (define spans-to-send
    (call-with-semaphore
     (tracer-flush-semaphore t)
     (lambda ()
       (define batch (unbox (tracer-buffer-queue t)))
       (set-box! (tracer-buffer-queue t) '())
       batch)))
  
  (if (null? spans-to-send)
      #t
      (with-handlers ([exn:fail? (lambda (_e)
                                   ;; Re-queue spans on transmission error
                                   (call-with-semaphore
                                    (tracer-flush-semaphore t)
                                    (lambda ()
                                      (set-box! (tracer-buffer-queue t)
                                                (append spans-to-send
                                                        (unbox (tracer-buffer-queue t))))))
                                   #f)])
        (define-values (in out) (tcp-connect (tracer-collector-host t)
                                             (tracer-collector-port t)))
        (define payload (jsexpr->string (map span->hash spans-to-send)))
        (define body-bytes (string->bytes/utf-8 payload))
        (fprintf out "POST /api/v1/spans HTTP/1.1\r\n")
        (fprintf out "Host: ~a:~a\r\n" (tracer-collector-host t) (tracer-collector-port t))
        (fprintf out "Content-Type: application/json\r\n")
        (fprintf out "Content-Length: ~a\r\n" (bytes-length body-bytes))
        (fprintf out "Connection: close\r\n\r\n")
        (write-bytes body-bytes out)
        (flush-output out)
        (define _line (read-line in 'any))
        (close-input-port in)
        (close-output-port out)
        #t)))

;; Injects active span into W3C traceparent header
(define (inject-w3c-traceparent s)
  (format "00-~a-~a-01" (span-trace-id s) (span-span-id s)))

;; Extracts trace-id and span-id from W3C traceparent header
(define (extract-w3c-parent header-str)
  (define parsed (parse-traceparent header-str))
  (if parsed
      (values (car parsed)
              (cadr parsed))
      (values #f #f)))

;; Convenient block syntax macro for tracing operations
(define-syntax with-span
  (syntax-rules ()
    [(_ (name-expr) body ...)
     (with-span (name-expr #:kind 'internal #:attributes (hash)) body ...)]
    [(_ (name-expr #:kind kind-expr) body ...)
     (with-span (name-expr #:kind kind-expr #:attributes (hash)) body ...)]
    [(_ (name-expr #:attributes attrs-expr) body ...)
     (with-span (name-expr #:kind 'internal #:attributes attrs-expr) body ...)]
    [(_ (name-expr #:kind kind-expr #:attributes attrs-expr) body ...)
     (let* ([t (or (current-tracer) (error 'with-span "No active tracer in (current-tracer)"))]
            [s (tracer-start-span t name-expr #:kind kind-expr #:attributes attrs-expr)])
       (with-handlers ([exn:fail? (lambda (e)
                                    (tracer-finish-span! t s
                                                         #:status 'error
                                                         #:status-message (exn-message e)
                                                         #:attributes (hash 'error.message (exn-message e)))
                                    (raise e))])
         (parameterize ([current-span s])
           (define result (let () body ...))
           (tracer-finish-span! t s #:status 'ok)
           result)))]))
