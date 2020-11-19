#lang racket/base

;;; vernier/anomaly/detector.rkt
;;; Multi-strategy anomaly detector combining statistical latency deviations
;;; with graph structural anti-pattern inspections.

(require racket/contract
         racket/list
         "../core/span.rkt"
         "../core/trace.rkt"
         "baseline.rkt"
         "pattern.rkt")

(provide
 (contract-out
  ;; Anomaly findings
  [struct span-latency-anomaly ([span-id string?]
                                [service-name string?]
                                [operation-name string?]
                                [duration-nanos exact-integer?]
                                [baseline-mean-nanos real?]
                                [baseline-p99-nanos real?]
                                [z-score real?])]
  
  [struct anomaly-report ([trace-id string?]
                          [latency-anomalies (listof span-latency-anomaly?)]
                          [structural-anomalies (listof pattern-anomaly?)]
                          [has-errors? boolean?]
                          [error-count exact-nonnegative-integer?])]
  
  ;; Registry and detection engine
  [struct anomaly-detector ([baselines hash?])]
  [make-anomaly-detector (-> anomaly-detector?)]
  [detector-record-span (-> anomaly-detector? span? anomaly-detector?)]
  [detector-record-trace (-> anomaly-detector? trace-record? anomaly-detector?)]
  [detector-analyze-trace (-> anomaly-detector? trace-record? anomaly-report?)]))

(struct span-latency-anomaly (span-id
                              service-name
                              operation-name
                              duration-nanos
                              baseline-mean-nanos
                              baseline-p99-nanos
                              z-score)
  #:transparent)

(struct anomaly-report (trace-id
                        latency-anomalies
                        structural-anomalies
                        has-errors?
                        error-count)
  #:transparent)

(struct anomaly-detector (baselines)
  #:transparent)

(define (make-anomaly-detector)
  (anomaly-detector (hash)))

;; Updates baseline profile for a single span
(define (detector-record-span det s)
  (define key (cons (span-service-name s) (span-name s)))
  (define current-base
    (hash-ref (anomaly-detector-baselines det) key
              (lambda () (make-operation-baseline (span-service-name s) (span-name s)))))
  (define updated-base (baseline-update current-base (span-duration s)))
  (struct-copy anomaly-detector det
               [baselines (hash-set (anomaly-detector-baselines det) key updated-base)]))

;; Updates baselines for all spans in a trace
(define (detector-record-trace det tr)
  (for/fold ([d det]) ([s (in-list (trace-record-spans tr))])
    (detector-record-span d s)))

;; Analyzes trace against existing baselines and structural rules
(define (detector-analyze-trace det tr)
  (define tid (trace-record-trace-id tr))
  (define latency-anomalies '())
  
  (for ([s (in-list (trace-record-spans tr))])
    (define key (cons (span-service-name s) (span-name s)))
    (define base (hash-ref (anomaly-detector-baselines det) key #f))
    (when (and base (baseline-latency-anomaly? base (span-duration s)))
      (define z (baseline-zscore base (span-duration s)))
      (set! latency-anomalies
            (cons (span-latency-anomaly
                   (span-span-id s)
                   (span-service-name s)
                   (span-name s)
                   (span-duration s)
                   (operation-baseline-mean-nanos base)
                   (operation-baseline-p99-nanos base)
                   z)
                  latency-anomalies))))
  
  (define structural (detect-structural-anomalies tr))
  (define has-err (trace-has-errors? tr))
  (define err-cnt (trace-record-error-count tr))
  
  (anomaly-report tid
                  (reverse latency-anomalies)
                  structural
                  has-err
                  err-cnt))
