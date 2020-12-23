#lang racket/base

;;; vernier/storage/column.rkt
;;; In-memory columnar storage segment for vectorized range queries and scans.
;;; Eliminates record-unpacking overhead during high-volume analytical aggregations.

(require racket/contract
         racket/vector
         racket/set
         "../core/span.rkt")

(provide
 (contract-out
  ;; Columnar block
  [struct column-block ([trace-ids (vectorof string?)]
                        [span-ids (vectorof string?)]
                        [start-times (vectorof exact-integer?)]
                        [durations (vectorof exact-integer?)]
                        [services (vectorof string?)]
                        [errors (vectorof boolean?)]
                        [length exact-nonnegative-integer?])]
  
  [make-column-block (-> (listof span?) column-block?)]
  [column-filter-duration (-> column-block? exact-integer? (or/c exact-integer? #f) (set/c string?))]
  [column-filter-time-range (-> column-block? exact-integer? exact-integer? (set/c string?))]
  [column-filter-service (-> column-block? string? (set/c string?))]
  [column-filter-errors (-> column-block? (set/c string?))]))

(struct column-block (trace-ids
                      span-ids
                      start-times
                      durations
                      services
                      errors
                      length)
  #:transparent)

;; Constructs a dense columnar block from a list of spans
(define (make-column-block spans)
  (define len (length spans))
  (define tids (make-vector len ""))
  (define sids (make-vector len ""))
  (define starts (make-vector len 0))
  (define durs (make-vector len 0))
  (define svcs (make-vector len ""))
  (define errs (make-vector len #f))
  
  (for ([s (in-list spans)] [i (in-range len)])
    (vector-set! tids i (span-trace-id s))
    (vector-set! sids i (span-span-id s))
    (vector-set! starts i (span-start-time s))
    (vector-set! durs i (span-duration s))
    (vector-set! svcs i (span-service-name s))
    (vector-set! errs i (span-error? s)))
  
  (column-block tids sids starts durs svcs errs len))

;; Fast vector scan for min-dur <= duration <= max-dur
(define (column-filter-duration cb min-dur max-dur)
  (define len (column-block-length cb))
  (define durs (column-block-durations cb))
  (define tids (column-block-trace-ids cb))
  
  (for/set ([i (in-range len)]
            #:when (let ([d (vector-ref durs i)])
                     (and (>= d min-dur)
                          (or (not max-dur) (<= d max-dur)))))
    (vector-ref tids i)))

;; Fast vector scan for start-time within interval [t0, t1]
(define (column-filter-time-range cb t0 t1)
  (define len (column-block-length cb))
  (define starts (column-block-start-times cb))
  (define tids (column-block-trace-ids cb))
  
  (for/set ([i (in-range len)]
            #:when (let ([t (vector-ref starts i)])
                     (and (>= t t0) (<= t t1))))
    (vector-ref tids i)))

;; Fast vector scan matching service name
(define (column-filter-service cb svc-name)
  (define len (column-block-length cb))
  (define svcs (column-block-services cb))
  (define tids (column-block-trace-ids cb))
  
  (for/set ([i (in-range len)]
            #:when (string=? (vector-ref svcs i) svc-name))
    (vector-ref tids i)))

;; Fast vector scan matching error spans
(define (column-filter-errors cb)
  (define len (column-block-length cb))
  (define errs (column-block-errors cb))
  (define tids (column-block-trace-ids cb))
  
  (for/set ([i (in-range len)]
            #:when (vector-ref errs i))
    (vector-ref tids i)))
