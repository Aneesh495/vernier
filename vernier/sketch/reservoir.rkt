#lang racket/base

;;; vernier/sketch/reservoir.rkt
;;; Exponentially decaying reservoir sampling with forward time weighting.
;;; Retains unbiased representative latency distributions over sliding time windows.

(require racket/contract
         racket/math
         racket/random
         racket/vector)

(provide
 (contract-out
  ;; Reservoir data structure
  [struct decaying-reservoir ([capacity exact-positive-integer?]
                              [alpha real?]
                              [start-time exact-integer?]
                              [count exact-nonnegative-integer?]
                              [samples vector?])]
  
  [make-decaying-reservoir (->* () (exact-positive-integer? real? exact-integer?) decaying-reservoir?)]
  [reservoir-insert (-> decaying-reservoir? any/c exact-integer? decaying-reservoir?)]
  [reservoir-insert! (-> (box/c decaying-reservoir?) any/c exact-integer? void?)]
  [reservoir-snapshot (-> decaying-reservoir? (listof any/c))]
  [reservoir-size (-> decaying-reservoir? exact-nonnegative-integer?)]))

;; Sample record stored in the priority vector: (cons priority value)
(struct decaying-reservoir (capacity alpha start-time count samples)
  #:transparent)

;; Constructs an exponentially decaying reservoir
;; alpha: decay rate parameter (e.g. 0.015 for ~5 min half-life)
(define (make-decaying-reservoir [capacity 1024]
                                 [alpha 0.015]
                                 [start-time (current-seconds)])
  (decaying-reservoir capacity
                      alpha
                      start-time
                      0
                      (vector)))

;; Functional insert of an item into decaying reservoir
(define (reservoir-insert res val timestamp-secs)
  (define cap (decaying-reservoir-capacity res))
  (define alpha (decaying-reservoir-alpha res))
  (define t0 (decaying-reservoir-start-time res))
  (define age (- timestamp-secs t0))
  (define weight (exp (* alpha age)))
  (define u (max 1e-9 (random)))
  (define priority (/ (- (log u)) (max 1e-9 weight)))
  
  (define current-samples (decaying-reservoir-samples res))
  (define current-len (vector-length current-samples))
  
  (cond
    [(< current-len cap)
     ;; Reservoir has room; append and sort
     (define new-vec (vector-append current-samples (vector (cons priority val))))
     (struct-copy decaying-reservoir res
                  [count (add1 (decaying-reservoir-count res))]
                  [samples (vector-sort new-vec < #:key car)])]
    [else
     ;; Reservoir is full; test against lowest priority (index 0)
     (define min-p (car (vector-ref current-samples 0)))
     (if (> priority min-p)
         ;; Evict lowest priority element and insert new one in sorted position
         (let* ([new-vec (vector-copy current-samples)]
                [_ (vector-set! new-vec 0 (cons priority val))]
                [sorted (vector-sort new-vec < #:key car)])
           (struct-copy decaying-reservoir res
                        [count (add1 (decaying-reservoir-count res))]
                        [samples sorted]))
         ;; Discard incoming element
         (struct-copy decaying-reservoir res
                      [count (add1 (decaying-reservoir-count res))]))]))

(define (reservoir-insert! box-res val timestamp-secs)
  (set-box! box-res (reservoir-insert (unbox box-res) val timestamp-secs)))

;; Extracts current uniform snapshot of representative values
(define (reservoir-snapshot res)
  (map cdr (vector->list (decaying-reservoir-samples res))))

(define (reservoir-size res)
  (vector-length (decaying-reservoir-samples res)))

;; Internal vector sorting helper
(define (vector-sort vec lt? #:key [key (lambda (x) x)])
  (list->vector (sort (vector->list vec) lt? #:key key)))
