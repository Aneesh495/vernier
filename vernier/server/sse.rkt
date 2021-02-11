#lang racket/base

;;; vernier/server/sse.rkt
;;; Server-Sent Events (SSE) real-time streaming hub for live trace broadcasts.
;;; Streams newly ingested distributed traces to connected web console subscribers.

(require racket/contract
         racket/list
         json
         "../core/span.rkt"
         "../core/trace.rkt")

(provide
 (contract-out
  ;; SSE Hub
  [struct sse-hub ([clients (box/c (listof output-port?))]
                   [lock semaphore?])]
  
  [make-sse-hub (-> sse-hub?)]
  [sse-subscribe! (-> sse-hub? output-port? void?)]
  [sse-unsubscribe! (-> sse-hub? output-port? void?)]
  [sse-broadcast-trace! (-> sse-hub? trace-record? void?)]
  [sse-client-count (-> sse-hub? exact-nonnegative-integer?)]))

(struct sse-hub (clients lock)
  #:transparent)

(define (make-sse-hub)
  (sse-hub (box '()) (make-semaphore 1)))

;; Subscribes a client output port to real-time events
(define (sse-subscribe! hub out)
  (call-with-semaphore
   (sse-hub-lock hub)
   (lambda ()
     ;; Send SSE initial headers
     (fprintf out "HTTP/1.1 200 OK\r\n")
     (fprintf out "Content-Type: text/event-stream\r\n")
     (fprintf out "Cache-Control: no-cache\r\n")
     (fprintf out "Connection: keep-alive\r\n")
     (fprintf out "Access-Control-Allow-Origin: *\r\n\r\n")
     (flush-output out)
     (set-box! (sse-hub-clients hub) (cons out (unbox (sse-hub-clients hub)))))))

;; Unsubscribes client port
(define (sse-unsubscribe! hub out)
  (call-with-semaphore
   (sse-hub-lock hub)
   (lambda ()
     (set-box! (sse-hub-clients hub)
               (filter (lambda (p) (not (eq? p out))) (unbox (sse-hub-clients hub)))))))

;; Broadcasts completed trace record to all connected SSE clients
(define (sse-broadcast-trace! hub tr)
  (call-with-semaphore
   (sse-hub-lock hub)
   (lambda ()
     (define summary (trace->summary tr))
     (define json-payload
       (jsexpr->string
        (hash 'type "trace"
              'trace_id (trace-summary-trace-id summary)
              'root_name (trace-summary-root-name summary)
              'root_service (trace-summary-root-service summary)
              'start_time (trace-summary-start-time summary)
              'duration_nanos (trace-summary-duration summary)
              'span_count (trace-summary-span-count summary)
              'error_count (trace-summary-error-count summary)
              'services (trace-summary-services summary))))
     
     (define dead-clients '())
     (for ([p (in-list (unbox (sse-hub-clients hub)))])
       (with-handlers ([exn:fail? (lambda (e) (set! dead-clients (cons p dead-clients)))])
         (fprintf p "event: trace\r\ndata: ~a\r\n\r\n" json-payload)
         (flush-output p)))
     
     ;; Purge disconnected clients
     (unless (null? dead-clients)
       (set-box! (sse-hub-clients hub)
                 (filter (lambda (p) (not (member p dead-clients)))
                         (unbox (sse-hub-clients hub))))))))

(define (sse-client-count hub)
  (length (unbox (sse-hub-clients hub))))
