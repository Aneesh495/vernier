#lang racket/base

;;; tests/unit/test_sketches.rkt
;;; Unit test suite for DDSketch quantiles, histograms, reservoir sampling,
;;; HyperLogLog cardinality estimation, and Bloom filters.

(require rackunit
         "../../vernier/sketch/quantile.rkt"
         "../../vernier/sketch/histogram.rkt"
         "../../vernier/sketch/reservoir.rkt"
         "../../vernier/sketch/hll.rkt"
         "../../vernier/sketch/bloom.rkt")

(test-case "DDSketch quantile estimation accuracy"
  (define sk (make-dd-sketch 0.01)) ;; 1% relative error bound
  ;; Insert values 1 to 1000
  (define populated
    (for/fold ([s sk]) ([i (in-range 1 1001)])
      (dd-sketch-insert s (* i 1.0))))
  
  (check-equal? (dd-sketch-count populated) 1000)
  (check-equal? (dd-sketch-min populated) 1.0)
  (check-equal? (dd-sketch-max populated) 1000.0)
  
  ;; Median (p50): expected ~500
  (define p50 (dd-sketch-quantile populated 0.5))
  (check-true (< (abs (- p50 500.0)) 20.0) "p50 must be within 1% relative error")
  
  ;; p99: expected ~990
  (define p99 (dd-sketch-quantile populated 0.99))
  (check-true (< (abs (- p99 990.0)) 25.0) "p99 must be within 1% relative error")
  
  ;; Merge test
  (define sk1 (for/fold ([s (make-dd-sketch 0.01)]) ([i (in-range 1 501)]) (dd-sketch-insert s (* i 1.0))))
  (define sk2 (for/fold ([s (make-dd-sketch 0.01)]) ([i (in-range 501 1001)]) (dd-sketch-insert s (* i 1.0))))
  (define merged (dd-sketch-merge sk1 sk2))
  (check-equal? (dd-sketch-count merged) 1000)
  (check-true (< (abs (- (dd-sketch-quantile merged 0.5) 500.0)) 20.0)))

(test-case "Histogram and distribution statistics"
  (define h (make-latency-histogram))
  (define populated-h
    (for/fold ([hist h]) ([val (list 5000.0 15000.0 500000.0 2000000.0 12000000.0)])
      (histogram-insert hist val)))
  
  (check-equal? (latency-histogram-total-count populated-h) 5)
  (check-true (> (histogram-mean populated-h) 0))
  (check-true (> (histogram-stddev populated-h) 0))
  (check-equal? (length (histogram-cdf populated-h)) 13)
  (check-equal? (length (histogram-pdf populated-h)) 13))

(test-case "Exponentially decaying reservoir sampling"
  (define res (make-decaying-reservoir 50 0.015 1000))
  (define filled-res
    (for/fold ([r res]) ([i (in-range 200)])
      (reservoir-insert r (format "item-~a" i) (+ 1000 i))))
  
  (check-equal? (reservoir-size filled-res) 50)
  (define snap (reservoir-snapshot filled-res))
  (check-equal? (length snap) 50))

(test-case "HyperLogLog cardinality estimation"
  (define hll (make-hyperloglog 10)) ;; 1024 registers
  (define populated-hll
    (for/fold ([h hll]) ([i (in-range 5000)])
      (hll-insert h (format "user-uuid-~a" i))))
  
  (define est-card (hll-cardinality populated-hll))
  ;; Within 5% error margin
  (check-true (< (abs (- est-card 5000)) 250)
              (format "HLL estimate (~a) should be close to 5000" est-card)))

(test-case "Bloom filter membership testing"
  (define bf (make-bloom-filter 500 0.01))
  (define populated-bf
    (for/fold ([b bf]) ([i (in-range 500)])
      (bloom-insert b (format "key:~a" i))))
  
  ;; No false negatives
  (for ([i (in-range 500)])
    (check-true (bloom-contains? populated-bf (format "key:~a" i))))
  
  ;; Low false positive rate on absent keys
  (define false-positives
    (for/sum ([i (in-range 500 1500)])
      (if (bloom-contains? populated-bf (format "absent-key:~a" i)) 1 0)))
  
  (check-true (< false-positives 30)
              (format "False positive count (~a/1000) must adhere to theoretical bound" false-positives)))
