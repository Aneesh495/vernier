#lang racket/base

;;; vernier/core/trace.rkt
;;; In-memory trace collection, parent-child indexing, and structural aggregation.
;;; Provides root discovery, error accounting, and multi-service trace analysis.

(require racket/contract
         racket/list
         racket/set
         "id.rkt"
         "time.rkt"
         "span.rkt")

(provide
 (contract-out
  ;; Trace summary record
  [struct trace-summary ([trace-id string?]
                         [root-name string?]
                         [root-service string?]
                         [start-time exact-integer?]
                         [duration exact-integer?]
                         [span-count exact-nonnegative-integer?]
                         [error-count exact-nonnegative-integer?]
                         [services (listof string?)])]
  
  ;; Core trace record
  [struct trace-record ([trace-id string?]
                        [spans (listof span?)]
                        [span-map hash?]
                        [parent-map hash?]
                        [root-span (or/c span? #f)]
                        [start-time exact-integer?]
                        [end-time exact-integer?]
                        [duration exact-integer?]
                        [services (listof string?)]
                        [error-count exact-nonnegative-integer?]
                        [complete? boolean?])]
  
  ;; Constructors and queries
  [make-trace (-> (listof span?) (or/c trace-record? #f))]
  [trace-add-span (-> trace-record? span? trace-record?)]
  [trace-find-span (-> trace-record? string? (or/c span? #f))]
  [trace-find-children (-> trace-record? string? (listof span?))]
  [trace-find-roots (-> trace-record? (listof span?))]
  [trace-has-errors? (-> trace-record? boolean?)]
  [trace->summary (-> trace-record? trace-summary?)]))

;; Summary structure for rapid dashboard queries
(struct trace-summary (trace-id
                       root-name
                       root-service
                       start-time
                       duration
                       span-count
                       error-count
                       services)
  #:transparent)

;; Full trace collection record with indexing
(struct trace-record (trace-id
                      spans
                      span-map
                      parent-map
                      root-span
                      start-time
                      end-time
                      duration
                      services
                      error-count
                      complete?)
  #:transparent)

;; Constructs a trace-record from an arbitrary list of spans
(define (make-trace span-list)
  (if (null? span-list)
      #f
      (let* ([first-span (car span-list)]
             [tid (span-trace-id first-span)])
        ;; Verify all spans share the same trace ID
        (for ([s (in-list span-list)])
          (unless (string=? (span-trace-id s) tid)
            (error 'make-trace "Spans belong to multiple traces: ~a vs ~a"
                   tid (span-trace-id s))))
        (build-trace-record tid span-list))))

;; Internal builder that indexes spans and computes aggregates
(define (build-trace-record tid span-list)
  (define s-map
    (for/hash ([s (in-list span-list)])
      (values (span-span-id s) s)))
  
  ;; Map parent-id -> list of child spans
  (define p-map
    (for/fold ([acc (hash)])
              ([s (in-list span-list)])
      (define pid (span-parent-id s))
      (if pid
          (hash-update acc pid (lambda (children) (cons s children)) '())
          acc)))
  
  ;; Roots are spans with no parent-id, or whose parent-id is absent from s-map
  (define potential-roots
    (filter (lambda (s)
              (define pid (span-parent-id s))
              (or (not pid) (not (hash-has-key? s-map pid))))
            span-list))
  
  ;; Prefer explicit root (pid is #f), or earliest starting potential root
  (define explicit-root (findf span-root? span-list))
  (define chosen-root
    (or explicit-root
        (and (not (null? potential-roots))
             (first (sort potential-roots < #:key span-start-time)))))
  
  (define start-t
    (apply min (map span-start-time span-list)))
  (define end-t
    (apply max (map span-end-time span-list)))
  (define total-dur (- end-t start-t))
  
  (define svc-set
    (for/set ([s (in-list span-list)])
      (span-service-name s)))
  (define svc-list (sort (set->list svc-set) string<?))
  
  (define errs
    (count span-error? span-list))
  
  ;; A trace is considered complete if it has an explicit root and no missing parents
  (define is-complete
    (and (not (null? span-list))
         (and explicit-root #t)
         (andmap (lambda (s)
                   (define pid (span-parent-id s))
                   (or (not pid) (hash-has-key? s-map pid)))
                 span-list)))
  
  (trace-record tid
                span-list
                s-map
                p-map
                chosen-root
                start-t
                end-t
                total-dur
                svc-list
                errs
                is-complete))

;; Appends a new span into an existing trace record
(define (trace-add-span tr s)
  (unless (string=? (span-trace-id s) (trace-record-trace-id tr))
    (error 'trace-add-span "Span trace-id ~a does not match trace ~a"
           (span-trace-id s) (trace-record-trace-id tr)))
  ;; Deduplicate or replace span if already present
  (define existing (filter (lambda (x) (not (string=? (span-span-id x) (span-span-id s))))
                           (trace-record-spans tr)))
  (build-trace-record (trace-record-trace-id tr) (cons s existing)))

;; Looks up span by ID
(define (trace-find-span tr sid)
  (hash-ref (trace-record-span-map tr) sid #f))

;; Returns all immediate children of span-id
(define (trace-find-children tr sid)
  (hash-ref (trace-record-parent-map tr) sid '()))

;; Finds all roots in the trace
(define (trace-find-roots tr)
  (filter (lambda (s)
            (define pid (span-parent-id s))
            (or (not pid)
                (not (hash-has-key? (trace-record-span-map tr) pid))))
          (trace-record-spans tr)))

;; True if any span in trace reported error
(define (trace-has-errors? tr)
  (> (trace-record-error-count tr) 0))

;; Produces compact summary for search result rendering
(define (trace->summary tr)
  (define r (trace-record-root-span tr))
  (trace-summary
   (trace-record-trace-id tr)
   (if r (span-name r) "unknown")
   (if r (span-service-name r) "unknown")
   (trace-record-start-time tr)
   (trace-record-duration tr)
   (length (trace-record-spans tr))
   (trace-record-error-count tr)
   (trace-record-services tr)))
