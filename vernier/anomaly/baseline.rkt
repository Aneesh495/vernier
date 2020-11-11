#lang racket/base

;;; vernier/anomaly/baseline.rkt
;;; Service and endpoint operational baselines using Welford online variance
;;; and streaming DDSketch quantile tracking.

(require racket/contract
         racket/math
         "../sketch/quantile.rkt")

(provide
 (contract-out
  ;; Baseline metrics record
  [struct operation-baseline ([service-name string?]
                              [operation-name string?]
                              [sample-count exact-nonnegative-integer?]
                              [mean-nanos real?]
                              [m2 real?]
                              [p50-nanos real?]
                              [p90-nanos real?]
                              [p99-nanos real?]
                              [sketch dd-sketch?]
                              [mean-child-count real?])]
  
  [make-operation-baseline (-> string? string? operation-baseline?)]
  [baseline-update (->* (operation-baseline? real?) (exact-nonnegative-integer?) operation-baseline?)]
  [baseline-update! (->* ((box/c operation-baseline?) real?) (exact-nonnegative-integer?) void?)]
  [baseline-stddev (-> operation-baseline? real?)]
  [baseline-zscore (-> operation-baseline? real? real?)]
  [baseline-latency-anomaly? (->* (operation-baseline? real?) (real?) boolean?)]))

(struct operation-baseline (service-name
                            operation-name
                            sample-count
                            mean-nanos
                            m2
                            p50-nanos
                            p90-nanos
                            p99-nanos
                            sketch
                            mean-child-count)
  #:transparent)

;; Constructs an empty baseline for a given service operation
(define (make-operation-baseline service-name operation-name)
  (operation-baseline service-name
                      operation-name
                      0
                      0.0
                      0.0
                      0.0
                      0.0
                      0.0
                      (make-dd-sketch 0.02)
                      0.0))

;; Updates baseline using Welford's algorithm for numerically stable variance
(define (baseline-update base duration-nanos [child-count 0])
  (define n (add1 (operation-baseline-sample-count base)))
  (define old-mean (operation-baseline-mean-nanos base))
  (define delta (- duration-nanos old-mean))
  (define new-mean (+ old-mean (/ delta n)))
  (define delta2 (- duration-nanos new-mean))
  (define new-m2 (+ (operation-baseline-m2 base) (* delta delta2)))
  
  (define new-sketch (dd-sketch-insert (operation-baseline-sketch base) duration-nanos))
  (define p50 (or (dd-sketch-quantile new-sketch 0.50) duration-nanos))
  (define p90 (or (dd-sketch-quantile new-sketch 0.90) duration-nanos))
  (define p99 (or (dd-sketch-quantile new-sketch 0.99) duration-nanos))
  
  (define old-child-mean (operation-baseline-mean-child-count base))
  (define new-child-mean (+ old-child-mean (/ (- child-count old-child-mean) n)))
  
  (operation-baseline (operation-baseline-service-name base)
                      (operation-baseline-operation-name base)
                      n
                      new-mean
                      new-m2
                      p50
                      p90
                      p99
                      new-sketch
                      new-child-mean))

(define (baseline-update! box-base duration-nanos [child-count 0])
  (set-box! box-base (baseline-update (unbox box-base) duration-nanos child-count)))

;; Computes standard deviation
(define (baseline-stddev base)
  (define n (operation-baseline-sample-count base))
  (if (< n 2)
      0.0
      (sqrt (/ (operation-baseline-m2 base) (sub1 n)))))

;; Computes robust Z-score with minimum standard deviation floor
(define (baseline-zscore base duration-nanos)
  (define mean (operation-baseline-mean-nanos base))
  (define raw-std (baseline-stddev base))
  (define std (max raw-std (* 0.05 (max 1.0 mean))))
  (/ (- duration-nanos mean) std))

;; Evaluates whether an observed duration constitutes a statistical anomaly
(define (baseline-latency-anomaly? base duration-nanos [z-threshold 3.0])
  (define n (operation-baseline-sample-count base))
  (cond
    [(< n 10) #f] ;; Insufficient samples for statistical significance
    [else
     (define z (baseline-zscore base duration-nanos))
     (or (> z z-threshold)
         (and (> duration-nanos (* 2.0 (operation-baseline-p99-nanos base)))
              (> duration-nanos (operation-baseline-mean-nanos base))))]))
