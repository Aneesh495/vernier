#lang racket/base

;;; vernier/sketch/histogram.rkt
;;; Configurable bucket histograms, cumulative distributions, and latency heatmaps.
;;; Provides frequency modeling across discrete latency tiers.

(require racket/contract
         racket/list
         racket/math
         racket/vector)

(provide
 (contract-out
  ;; Latency histogram record
  [struct latency-histogram ([boundaries (vectorof real?)]
                             [counts (vectorof exact-nonnegative-integer?)]
                             [total-count exact-nonnegative-integer?]
                             [sum-val real?]
                             [sum-squares real?])]
  
  [default-latency-boundaries (vectorof real?)]
  [make-latency-histogram (->* () ((vectorof real?)) latency-histogram?)]
  [histogram-insert (-> latency-histogram? real? latency-histogram?)]
  [histogram-insert! (-> (box/c latency-histogram?) real? void?)]
  [histogram-mean (-> latency-histogram? real?)]
  [histogram-variance (-> latency-histogram? real?)]
  [histogram-stddev (-> latency-histogram? real?)]
  [histogram-cdf (-> latency-histogram? (listof (cons/c real? real?)))]
  [histogram-pdf (-> latency-histogram? (listof (cons/c real? real?)))]
  
  ;; Heatmap matrix
  [struct heatmap-matrix ([time-window-nanos exact-integer?]
                          [buckets (vectorof real?)]
                          [grid hash?])]
  [make-heatmap-matrix (->* (exact-integer?) ((vectorof real?)) heatmap-matrix?)]
  [heatmap-record (-> heatmap-matrix? exact-integer? real? heatmap-matrix?)]
  [heatmap->rows (-> heatmap-matrix? (listof hash?))]))

;; Standard nanosecond latency boundaries: 10µs to 10s
(define default-latency-boundaries
  (vector 10000.0         ; 10 µs
          50000.0         ; 50 µs
          250000.0        ; 250 µs
          1000000.0       ; 1 ms
          5000000.0       ; 5 ms
          25000000.0      ; 25 ms
          100000000.0     ; 100 ms
          250000000.0     ; 250 ms
          500000000.0     ; 500 ms
          1000000000.0    ; 1 s
          2500000000.0    ; 2.5 s
          10000000000.0)) ; 10 s

(struct latency-histogram (boundaries counts total-count sum-val sum-squares)
  #:transparent)

;; Constructs empty latency histogram
(define (make-latency-histogram [bounds default-latency-boundaries])
  (define num-buckets (add1 (vector-length bounds)))
  (latency-histogram bounds
                     (make-vector num-buckets 0)
                     0
                     0.0
                     0.0))

;; Binary search for bucket index
(define (find-bucket-index bounds val)
  (define len (vector-length bounds))
  (let loop ([low 0] [high len])
    (if (< low high)
        (let ([mid (quotient (+ low high) 2)])
          (if (<= val (vector-ref bounds mid))
              (loop low mid)
              (loop (add1 mid) high)))
        low)))

;; Functional insertion into histogram
(define (histogram-insert h val)
  (define idx (find-bucket-index (latency-histogram-boundaries h) val))
  (define new-counts (vector-copy (latency-histogram-counts h)))
  (vector-set! new-counts idx (add1 (vector-ref new-counts idx)))
  (latency-histogram (latency-histogram-boundaries h)
                     new-counts
                     (add1 (latency-histogram-total-count h))
                     (+ (latency-histogram-sum-val h) val)
                     (+ (latency-histogram-sum-squares h) (* val val))))

;; Mutable in-place update wrapper
(define (histogram-insert! box-h val)
  (set-box! box-h (histogram-insert (unbox box-h) val)))

(define (histogram-mean h)
  (if (zero? (latency-histogram-total-count h))
      0.0
      (/ (latency-histogram-sum-val h) (latency-histogram-total-count h))))

(define (histogram-variance h)
  (define n (latency-histogram-total-count h))
  (if (<= n 1)
      0.0
      (let* ([mean (histogram-mean h)]
             [var (- (/ (latency-histogram-sum-squares h) n) (* mean mean))])
        (max 0.0 var))))

(define (histogram-stddev h)
  (sqrt (histogram-variance h)))

;; Computes Cumulative Distribution Function pairs: (boundary . cumulative_prob)
(define (histogram-cdf h)
  (define total (latency-histogram-total-count h))
  (if (zero? total)
      '()
      (let* ([bounds (latency-histogram-boundaries h)]
             [counts (latency-histogram-counts h)]
             [len (vector-length bounds)])
        (let loop ([i 0] [accum 0] [res '()])
          (if (= i len)
              (reverse (cons (cons +inf.0 1.0) res))
              (let* ([c (vector-ref counts i)]
                     [new-accum (+ accum c)]
                     [prob (/ new-accum (* 1.0 total))])
                (loop (add1 i) new-accum (cons (cons (vector-ref bounds i) prob) res))))))))

;; Computes Probability Density Function pairs: (boundary . probability)
(define (histogram-pdf h)
  (define total (latency-histogram-total-count h))
  (if (zero? total)
      '()
      (let* ([bounds (latency-histogram-boundaries h)]
             [counts (latency-histogram-counts h)]
             [len (vector-length bounds)])
        (for/list ([i (in-range (add1 len))])
          (define bound (if (< i len) (vector-ref bounds i) +inf.0))
          (define prob (/ (vector-ref counts i) (* 1.0 total)))
          (cons bound prob)))))

;; Heatmap tracking across discrete time intervals
(struct heatmap-matrix (time-window-nanos buckets grid)
  #:transparent)

(define (make-heatmap-matrix [window-nanos 1000000000] [buckets default-latency-boundaries])
  (heatmap-matrix window-nanos buckets (hash)))

;; Records a latency observation at given timestamp into heatmap matrix
(define (heatmap-record hm timestamp-nanos latency-nanos)
  (define slot (quotient timestamp-nanos (heatmap-matrix-time-window-nanos hm)))
  (define b-idx (find-bucket-index (heatmap-matrix-buckets hm) latency-nanos))
  (define current-grid (heatmap-matrix-grid hm))
  (define slot-hist (hash-ref current-grid slot (lambda () (make-vector (add1 (vector-length (heatmap-matrix-buckets hm))) 0))))
  (define new-hist (vector-copy slot-hist))
  (vector-set! new-hist b-idx (add1 (vector-ref new-hist b-idx)))
  (struct-copy heatmap-matrix hm
               [grid (hash-set current-grid slot new-hist)]))

;; Converts heatmap into serializable rows
(define (heatmap->rows hm)
  (define sorted-slots (sort (hash-keys (heatmap-matrix-grid hm)) <))
  (for/list ([slot (in-list sorted-slots)])
    (define counts (hash-ref (heatmap-matrix-grid hm) slot))
    (hash 'time_slot slot
          'timestamp_start (* slot (heatmap-matrix-time-window-nanos hm))
          'counts (vector->list counts))))
