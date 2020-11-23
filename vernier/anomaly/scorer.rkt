#lang racket/base

;;; vernier/anomaly/scorer.rkt
;;; Composite anomaly scoring and health categorization.
;;; Synthesizes statistical, structural, and error indicators into a unified score.

(require racket/contract
         racket/list
         "detector.rkt"
         "pattern.rkt"
         "../core/trace.rkt")

(provide
 (contract-out
  ;; Health classification
  [struct composite-score ([score real?]                         ; 0.0 to 1.0
                           [health (or/c 'healthy 'degraded 'critical)]
                           [primary-issue string?]
                           [contributing-factors (listof string?)])]
  
  [score-anomaly-report (-> anomaly-report? trace-record? composite-score?)]))

(struct composite-score (score health primary-issue contributing-factors)
  #:transparent)

;; Evaluates an anomaly-report and returns a normalized score and health diagnosis
(define (score-anomaly-report rep tr)
  (define factors '())
  (define total-weight 0.0)
  
  ;; Factor 1: Explicit span errors (weight up to 0.45)
  (define error-score
    (if (anomaly-report-has-errors? rep)
        (let ([cnt (anomaly-report-error-count rep)])
          (set! factors (cons (format "~a span error(s) detected in execution graph" cnt) factors))
          (min 0.45 (+ 0.30 (* 0.05 (min cnt 3)))))
        0.0))
  (set! total-weight (+ total-weight error-score))
  
  ;; Factor 2: Latency anomalies (weight up to 0.35)
  (define lat-anomalies (anomaly-report-latency-anomalies rep))
  (define latency-score
    (if (null? lat-anomalies)
        0.0
        (let* ([max-z (apply max (map span-latency-anomaly-z-score lat-anomalies))]
               [clamped-z (min 10.0 (max 0.0 max-z))]
               [ratio (/ clamped-z 10.0)])
          (set! factors (cons (format "~a span(s) exhibit statistical latency anomalies (max Z: ~a)"
                                      (length lat-anomalies)
                                      (/ (round (* max-z 10.0)) 10.0))
                              factors))
          (* ratio 0.35))))
  (set! total-weight (+ total-weight latency-score))
  
  ;; Factor 3: Structural anti-patterns (weight up to 0.30)
  (define struct-anomalies (anomaly-report-structural-anomalies rep))
  (define struct-score
    (if (null? struct-anomalies)
        0.0
        (let ([score (min 0.30 (* 0.15 (length struct-anomalies)))])
          (for ([sa (in-list struct-anomalies)])
            (set! factors (cons (pattern-anomaly-description sa) factors)))
          score)))
  (set! total-weight (+ total-weight struct-score))
  
  (define final-score (min 1.0 (/ (round (* total-weight 100.0)) 100.0)))
  
  (define health
    (cond
      [(>= final-score 0.65) 'critical]
      [(>= final-score 0.25) 'degraded]
      [else 'healthy]))
  
  (define primary
    (cond
      [(anomaly-report-has-errors? rep) "Downstream service error during execution"]
      [(not (null? lat-anomalies)) "Severe latency tail deviation on critical path"]
      [(not (null? struct-anomalies)) "Structural inefficiency (e.g. N+1 query loop or fan-out)"]
      [else "Execution conforms to normal baseline"]))
  
  (composite-score final-score health primary (reverse factors)))
