#lang racket/base

;;; vernier/sketch/quantile.rkt
;;; DDSketch streaming quantile estimator with bounded relative error.
;;; Provides nanosecond latency percentile tracking in constant memory.

(require racket/contract
         racket/math)

(provide
 (contract-out
  ;; DDSketch data structure
  [struct dd-sketch ([alpha real?]
                     [gamma real?]
                     [multiplier real?]
                     [zero-count exact-nonnegative-integer?]
                     [count exact-nonnegative-integer?]
                     [min-val (or/c real? #f)]
                     [max-val (or/c real? #f)]
                     [sum-val real?]
                     [positive-buckets hash?])]
  
  ;; Constructors and operations
  [make-dd-sketch (->* () (real?) dd-sketch?)]
  [dd-sketch-insert (-> dd-sketch? real? dd-sketch?)]
  [dd-sketch-insert! (-> (box/c dd-sketch?) real? void?)]
  [dd-sketch-quantile (-> dd-sketch? (real-in 0.0 1.0) (or/c real? #f))]
  [dd-sketch-merge (-> dd-sketch? dd-sketch? dd-sketch?)]
  [dd-sketch-mean (-> dd-sketch? real?)]
  [dd-sketch-min (-> dd-sketch? (or/c real? #f))]
  [dd-sketch-max (-> dd-sketch? (or/c real? #f))]))

(struct dd-sketch (alpha
                   gamma
                   multiplier
                   zero-count
                   count
                   min-val
                   max-val
                   sum-val
                   positive-buckets)
  #:transparent)

;; Constructs a new DDSketch with a given relative error bound alpha (default 0.01 = 1%)
(define (make-dd-sketch [alpha 0.01])
  (unless (and (> alpha 0.0) (< alpha 1.0))
    (error 'make-dd-sketch "alpha must be between 0 and 1, got ~a" alpha))
  (define gamma (/ (+ 1.0 alpha) (- 1.0 alpha)))
  (define multiplier (/ 1.0 (log gamma)))
  (dd-sketch alpha
             gamma
             multiplier
             0
             0
             #f
             #f
             0.0
             (hash)))

;; Maps a positive real value to a discrete bucket index
(define (val->index val mult)
  (exact-floor (* (log val) mult)))

;; Maps a bucket index back to the representative median value
(define (index->val idx gamma)
  (* 0.5 (+ 1.0 gamma) (expt gamma idx)))

;; Functional insert of a numeric value into DDSketch
(define (dd-sketch-insert sk v)
  (cond
    [(<= v 0.0)
     ;; Values <= 0 are accounted in zero-count
     (struct-copy dd-sketch sk
                  [zero-count (add1 (dd-sketch-zero-count sk))]
                  [count (add1 (dd-sketch-count sk))]
                  [min-val (if (dd-sketch-min-val sk) (min (dd-sketch-min-val sk) v) v)]
                  [max-val (if (dd-sketch-max-val sk) (max (dd-sketch-max-val sk) v) v)]
                  [sum-val (+ (dd-sketch-sum-val sk) v)])]
    [else
     (define idx (val->index v (dd-sketch-multiplier sk)))
     (define new-buckets
       (hash-update (dd-sketch-positive-buckets sk) idx add1 0))
     (struct-copy dd-sketch sk
                  [count (add1 (dd-sketch-count sk))]
                  [min-val (if (dd-sketch-min-val sk) (min (dd-sketch-min-val sk) v) v)]
                  [max-val (if (dd-sketch-max-val sk) (max (dd-sketch-max-val sk) v) v)]
                  [sum-val (+ (dd-sketch-sum-val sk) v)]
                  [positive-buckets new-buckets])]))

;; Mutable in-place update wrapper
(define (dd-sketch-insert! box-sk v)
  (set-box! box-sk (dd-sketch-insert (unbox box-sk) v)))

;; Queries the sketch for a given quantile q in [0.0, 1.0]
(define (dd-sketch-quantile sk q)
  (define total (dd-sketch-count sk))
  (cond
    [(zero? total) #f]
    [(<= q 0.0) (dd-sketch-min-val sk)]
    [(>= q 1.0) (dd-sketch-max-val sk)]
    [else
     (define rank (exact-ceiling (* q total)))
     (if (<= rank (dd-sketch-zero-count sk))
         0.0
         (let* ([target-rank (- rank (dd-sketch-zero-count sk))]
                [sorted-indices (sort (hash-keys (dd-sketch-positive-buckets sk)) <)])
           (let search ([indices sorted-indices] [accum 0])
             (if (null? indices)
                 (dd-sketch-max-val sk)
                 (let* ([idx (car indices)]
                        [bcount (hash-ref (dd-sketch-positive-buckets sk) idx)]
                        [new-accum (+ accum bcount)])
                   (if (>= new-accum target-rank)
                       (index->val idx (dd-sketch-gamma sk))
                       (search (cdr indices) new-accum)))))))]))

;; Merges two DDSketches with compatible alpha parameters
(define (dd-sketch-merge sk1 sk2)
  (unless (< (abs (- (dd-sketch-alpha sk1) (dd-sketch-alpha sk2))) 1e-6)
    (error 'dd-sketch-merge "Cannot merge sketches with differing alpha: ~a vs ~a"
           (dd-sketch-alpha sk1) (dd-sketch-alpha sk2)))
  (define merged-buckets
    (for/fold ([acc (dd-sketch-positive-buckets sk1)])
              ([(idx count) (in-hash (dd-sketch-positive-buckets sk2))])
      (hash-update acc idx (lambda (c) (+ c count)) 0)))
  (define new-min
    (cond
      [(and (dd-sketch-min-val sk1) (dd-sketch-min-val sk2))
       (min (dd-sketch-min-val sk1) (dd-sketch-min-val sk2))]
      [else (or (dd-sketch-min-val sk1) (dd-sketch-min-val sk2))]))
  (define new-max
    (cond
      [(and (dd-sketch-max-val sk1) (dd-sketch-max-val sk2))
       (max (dd-sketch-max-val sk1) (dd-sketch-max-val sk2))]
      [else (or (dd-sketch-max-val sk1) (dd-sketch-max-val sk2))]))
  (dd-sketch (dd-sketch-alpha sk1)
             (dd-sketch-gamma sk1)
             (dd-sketch-multiplier sk1)
             (+ (dd-sketch-zero-count sk1) (dd-sketch-zero-count sk2))
             (+ (dd-sketch-count sk1) (dd-sketch-count sk2))
             new-min
             new-max
             (+ (dd-sketch-sum-val sk1) (dd-sketch-sum-val sk2))
             merged-buckets))

;; Returns statistical mean of inserted values
(define (dd-sketch-mean sk)
  (if (zero? (dd-sketch-count sk))
      0.0
      (/ (dd-sketch-sum-val sk) (dd-sketch-count sk))))

(define (dd-sketch-min sk) (dd-sketch-min-val sk))
(define (dd-sketch-max sk) (dd-sketch-max-val sk))
