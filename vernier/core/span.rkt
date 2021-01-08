#lang racket/base

;;; vernier/core/span.rkt
;;; In-memory span representation, span events, links, and lifecycle metadata.
;;; Complies with distributed tracing OpenTelemetry and W3C data models.

(require racket/contract
         racket/match
         "id.rkt"
         "time.rkt")

(provide
 (contract-out
  ;; Span event and link structs
  [struct span-event ([name string?]
                      [timestamp exact-integer?]
                      [attributes hash?])]
  [make-span-event (->* (string?) (exact-integer? hash?) span-event?)]
  
  [struct span-link ([trace-id string?]
                     [span-id string?]
                     [attributes hash?])]
  [make-span-link (->* (string? string?) (hash?) span-link?)]
  
  ;; Core span struct
  [struct span ([trace-id string?]
                [span-id string?]
                [parent-id (or/c string? #f)]
                [name string?]
                [service-name string?]
                [kind (or/c 'internal 'server 'client 'producer 'consumer)]
                [start-time exact-integer?]
                [end-time exact-integer?]
                [duration exact-integer?]
                [status-code (or/c 'ok 'error 'unset)]
                [status-message string?]
                [attributes hash?]
                [events (listof span-event?)]
                [links (listof span-link?)])]
  
  ;; Constructors and builders
  [make-span (->* (#:trace-id string?
                   #:span-id string?
                   #:name string?
                   #:service-name string?
                   #:start-time exact-integer?
                   #:end-time exact-integer?)
                  (#:parent-id (or/c string? #f)
                   #:kind (or/c 'internal 'server 'client 'producer 'consumer)
                   #:status-code (or/c 'ok 'error 'unset)
                   #:status-message string?
                   #:attributes hash?
                   #:events (listof span-event?)
                   #:links (listof span-link?))
                  span?)]
  
  ;; Predicates and accessors
  [span-root? (-> span? boolean?)]
  [span-error? (-> span? boolean?)]
  [span-server? (-> span? boolean?)]
  [span-client? (-> span? boolean?)]
  
  ;; Functional modification
  [span-set-attribute (-> span? any/c any/c span?)]
  [span-get-attribute (->* (span? any/c) (any/c) any/c)]
  [span-add-event (-> span? span-event? span?)]
  [span-add-link (-> span? span-link? span?)]
  [span-set-status (->* (span? (or/c 'ok 'error 'unset)) (string?) span?)]
  
  ;; Serialization mapping
  [span->hash (-> span? hash?)]
  [hash->span (-> hash? span?)]))

;; Represents an instantaneous point-in-time event within a span
(struct span-event (name timestamp attributes)
  #:transparent)

(define (make-span-event name [timestamp (current-time-nanos)] [attributes (hasheq)])
  (span-event name timestamp attributes))

;; Represents a causal link to another span (potentially in another trace)
(struct span-link (trace-id span-id attributes)
  #:transparent)

(define (make-span-link trace-id span-id [attributes (hasheq)])
  (unless (valid-trace-id? trace-id)
    (error 'make-span-link "Invalid trace-id for link: ~a" trace-id))
  (unless (valid-span-id? span-id)
    (error 'make-span-link "Invalid span-id for link: ~a" span-id))
  (span-link trace-id span-id attributes))

;; Core Span Data Structure
(struct span (trace-id
              span-id
              parent-id
              name
              service-name
              kind
              start-time
              end-time
              duration
              status-code
              status-message
              attributes
              events
              links)
  #:transparent
  #:guard (lambda (trace-id span-id parent-id name service-name kind
                            start-time end-time duration status-code status-message
                            attributes events links type-name)
            (unless (valid-trace-id? trace-id)
              (error type-name "Invalid trace-id: ~a" trace-id))
            (unless (valid-span-id? span-id)
              (error type-name "Invalid span-id: ~a" span-id))
            (when parent-id
              (unless (valid-span-id? parent-id)
                (error type-name "Invalid parent-id: ~a" parent-id)))
            (unless (<= start-time end-time)
              (error type-name "Span start-time (~a) cannot exceed end-time (~a)" start-time end-time))
            (values trace-id span-id parent-id name service-name kind
                    start-time end-time duration status-code status-message
                    attributes events links)))

;; Keyword-based constructor
(define (make-span #:trace-id trace-id
                   #:span-id span-id
                   #:name name
                   #:service-name service-name
                   #:start-time start-time
                   #:end-time end-time
                   #:parent-id [parent-id #f]
                   #:kind [kind 'internal]
                   #:status-code [status-code 'unset]
                   #:status-message [status-message ""]
                   #:attributes [attributes (hasheq)]
                   #:events [events '()]
                   #:links [links '()])
  (define duration (- end-time start-time))
  (span trace-id
        span-id
        parent-id
        name
        service-name
        kind
        start-time
        end-time
        duration
        status-code
        status-message
        attributes
        events
        links))

;; Predicates
(define (span-root? s)
  (not (span-parent-id s)))

(define (span-error? s)
  (eq? (span-status-code s) 'error))

(define (span-server? s)
  (eq? (span-kind s) 'server))

(define (span-client? s)
  (eq? (span-kind s) 'client))

;; Functional attribute manipulation
(define (span-set-attribute s key val)
  (define updated (hash-set (span-attributes s) key val))
  (struct-copy span s [attributes updated]))

(define (span-get-attribute s key [default #f])
  (or (hash-ref (span-attributes s) key #f)
      (and (string? key) (hash-ref (span-attributes s) (string->symbol key) #f))
      (and (symbol? key) (hash-ref (span-attributes s) (symbol->string key) #f))
      default))

;; Event and link appenders
(define (span-add-event s evt)
  (define updated (append (span-events s) (list evt)))
  (struct-copy span s [events updated]))

(define (span-add-link s lnk)
  (define updated (append (span-links s) (list lnk)))
  (struct-copy span s [links updated]))

(define (span-set-status s code [msg ""])
  (struct-copy span s [status-code code] [status-message msg]))

;; Serialization to dictionary
(define (span->hash s)
  (hash
   'trace_id (span-trace-id s)
   'span_id (span-span-id s)
   'parent_id (or (span-parent-id s) (json-null))
   'name (span-name s)
   'service_name (span-service-name s)
   'kind (symbol->string (span-kind s))
   'start_time (span-start-time s)
   'end_time (span-end-time s)
   'duration (span-duration s)
   'status_code (symbol->string (span-status-code s))
   'status_message (span-status-message s)
   'attributes (for/hasheq ([(k v) (in-hash (span-attributes s))])
                 (values (if (symbol? k) k (string->symbol (format "~a" k))) v))
   'events (map (lambda (e)
                  (hash 'name (span-event-name e)
                        'timestamp (span-event-timestamp e)
                        'attributes (span-event-attributes e)))
                (span-events s))
   'links (map (lambda (l)
                 (hash 'trace_id (span-link-trace-id l)
                       'span_id (span-link-span-id l)
                       'attributes (span-link-attributes l)))
               (span-links s))))

;; Deserialization from dictionary
(define (hash->span h)
  (define trace-id (hash-ref h 'trace_id (lambda () (hash-ref h "trace_id"))))
  (define span-id (hash-ref h 'span_id (lambda () (hash-ref h "span_id"))))
  (define raw-parent (hash-ref h 'parent_id (lambda () (hash-ref h "parent_id" #f))))
  (define parent-id (if (or (not raw-parent) (json-null? raw-parent) (string=? raw-parent "")) #f raw-parent))
  (define name (hash-ref h 'name (lambda () (hash-ref h "name"))))
  (define service (hash-ref h 'service_name (lambda () (hash-ref h "service_name"))))
  (define kind-raw (hash-ref h 'kind (lambda () (hash-ref h "kind" "internal"))))
  (define kind (if (symbol? kind-raw) kind-raw (string->symbol (string-downcase kind-raw))))
  (define start-time (hash-ref h 'start_time (lambda () (hash-ref h "start_time"))))
  (define end-time (hash-ref h 'end_time (lambda () (hash-ref h "end_time"))))
  (define status-raw (hash-ref h 'status_code (lambda () (hash-ref h "status_code" "unset"))))
  (define status (if (symbol? status-raw) status-raw (string->symbol (string-downcase status-raw))))
  (define status-msg (hash-ref h 'status_message (lambda () (hash-ref h "status_message" ""))))
  (define attrs (hash-ref h 'attributes (lambda () (hash-ref h "attributes" (hasheq)))))
  (define raw-events (hash-ref h 'events (lambda () (hash-ref h "events" '()))))
  (define events
    (map (lambda (eh)
           (make-span-event
            (hash-ref eh 'name (lambda () (hash-ref eh "name")))
            (hash-ref eh 'timestamp (lambda () (hash-ref eh "timestamp")))
            (hash-ref eh 'attributes (lambda () (hash-ref eh "attributes" (hasheq))))))
         raw-events))
  (define raw-links (hash-ref h 'links (lambda () (hash-ref h "links" '()))))
  (define links
    (map (lambda (lh)
           (make-span-link
            (hash-ref lh 'trace_id (lambda () (hash-ref lh "trace_id")))
            (hash-ref lh 'span_id (lambda () (hash-ref lh "span_id")))
            (hash-ref lh 'attributes (lambda () (hash-ref lh "attributes" (hasheq))))))
         raw-links))
  (make-span #:trace-id trace-id
             #:span-id span-id
             #:parent-id parent-id
             #:name name
             #:service-name service
             #:kind kind
             #:start-time start-time
             #:end-time end-time
             #:status-code status
             #:status-message status-msg
             #:attributes attrs
             #:events events
             #:links links))

;; JSON null compatibility sentinel
(define (json-null) 'null)
(define (json-null? x) (or (eq? x 'null) (eq? x (void))))
