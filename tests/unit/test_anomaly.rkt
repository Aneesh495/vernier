#lang racket/base

;;; tests/unit/test_anomaly.rkt
;;; Unit test suite for operational baselines, N+1 query detectors,
;;; statistical anomaly identification, and composite scoring.

(require rackunit
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/anomaly/baseline.rkt"
         "../../vernier/anomaly/pattern.rkt"
         "../../vernier/anomaly/detector.rkt"
         "../../vernier/anomaly/scorer.rkt")

(test-case "Welford baseline update and variance calculations"
  (define base (make-operation-baseline "payment-service" "Authorize"))
  (define updated
    (for/fold ([b base]) ([val (list 100.0 102.0 98.0 101.0 99.0 100.0 103.0 97.0 101.0 100.0)])
      (baseline-update b val)))
  
  (check-equal? (operation-baseline-sample-count updated) 10)
  (check-true (< (abs (- (operation-baseline-mean-nanos updated) 100.1)) 0.5))
  (check-true (> (baseline-stddev updated) 0))
  (check-true (< (baseline-stddev updated) 3.0))
  
  ;; Normal value has small Z-score
  (check-true (< (abs (baseline-zscore updated 100.0)) 1.0))
  ;; Huge value has massive Z-score
  (check-true (> (baseline-zscore updated 500.0) 10.0) "Large value must yield high Z-score")
  (check-true (baseline-latency-anomaly? updated 500.0)))

(test-case "N+1 query pattern detection"
  (define tid (make-trace-id))
  (define root-id (make-span-id))
  (define root-span
    (make-span #:trace-id tid #:span-id root-id #:name "RenderCatalog"
               #:service-name "web" #:start-time 1000 #:end-time 5000))
  
  ;; Create 10 repeated child queries to db: "SELECT * FROM items WHERE id = ?"
  (define children
    (for/list ([i (in-range 10)])
      (make-span #:trace-id tid
                 #:span-id (make-span-id)
                 #:parent-id root-id
                 #:name "SELECT * FROM items WHERE id = ?"
                 #:service-name "postgres"
                 #:start-time (+ 1100 (* i 300))
                 #:end-time (+ 1300 (* i 300)))))
  
  (define tr (make-trace (cons root-span children)))
  (define patterns (detect-structural-anomalies tr))
  (check-true (> (length patterns) 0))
  
  (define n1-pat (findf (lambda (p) (eq? (pattern-anomaly-kind p) 'n-plus-one)) patterns))
  (check-not-false n1-pat)
  (check-equal? (pattern-anomaly-affected-count n1-pat) 10)
  (check-equal? (pattern-anomaly-service-name n1-pat) "web"))

(test-case "Anomaly detector and composite scoring"
  (define det (make-anomaly-detector))
  (define tid (make-trace-id))
  
  ;; Train detector on 20 normal traces
  (define trained-det
    (for/fold ([d det]) ([i (in-range 20)])
      (define s (make-span #:trace-id (make-trace-id)
                           #:span-id (make-span-id)
                           #:name "Search"
                           #:service-name "catalog"
                           #:start-time 1000
                           #:end-time (+ 1000 50000000))) ;; ~50ms
      (detector-record-span d s)))
  
  ;; Test normal trace
  (define s-normal (make-span #:trace-id tid
                              #:span-id (make-span-id)
                              #:name "Search"
                              #:service-name "catalog"
                              #:start-time 1000
                              #:end-time (+ 1000 52000000)))
  (define tr-normal (make-trace (list s-normal)))
  (define rep-normal (detector-analyze-trace trained-det tr-normal))
  (check-equal? (length (anomaly-report-latency-anomalies rep-normal)) 0)
  (define score-normal (score-anomaly-report rep-normal tr-normal))
  (check-equal? (composite-score-health score-normal) 'healthy)
  
  ;; Test anomalous trace with massive latency spike
  (define s-slow (make-span #:trace-id tid
                            #:span-id (make-span-id)
                            #:name "Search"
                            #:service-name "catalog"
                            #:start-time 1000
                            #:end-time (+ 1000 950000000))) ;; ~950ms (nearly 20x mean)
  (define tr-slow (make-trace (list s-slow)))
  (define rep-slow (detector-analyze-trace trained-det tr-slow))
  (check-equal? (length (anomaly-report-latency-anomalies rep-slow)) 1)
  (define score-slow (score-anomaly-report rep-slow tr-slow))
  (check-true (> (composite-score-score score-slow) 0.2)))
