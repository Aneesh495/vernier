#lang racket/base

;;; vernier/sampler/rules.rkt
;;; Declarative tail-based sampling rules DSL and predicate evaluation.
;;; Guarantees retention of high-value anomalies and error traces.

(require racket/contract
         racket/list
         racket/match
         racket/string
         "../core/span.rkt"
         "../core/trace.rkt")

(provide
 (contract-out
  ;; Sampling rule structures
  [struct sample-rule ([name string?]
                       [predicate (-> trace-record? boolean?)]
                       [sample-rate real?])]
  
  ;; Built-in rule constructors
  [make-error-rule (->* () (real?) sample-rule?)]
  [make-latency-threshold-rule (-> exact-integer? real? string? sample-rule?)]
  [make-service-match-rule (-> string? real? sample-rule?)]
  [make-attribute-match-rule (-> any/c any/c real? string? sample-rule?)]
  [make-probabilistic-rule (-> real? sample-rule?)]
  
  ;; Rule set evaluation
  [struct sampling-policy ([rules (listof sample-rule?)]
                           [default-rate real?])]
  [make-sampling-policy (->* () ((listof sample-rule?) real?) sampling-policy?)]
  [default-production-policy sampling-policy?]))

(struct sample-rule (name predicate sample-rate)
  #:transparent)

(struct sampling-policy (rules default-rate)
  #:transparent)

;; Rule: match if trace contains any error spans
(define (make-error-rule [rate 1.0])
  (sample-rule
   "retain-all-errors"
   (lambda (tr) (trace-has-errors? tr))
   rate))

;; Rule: match if total trace duration exceeds threshold in nanoseconds
(define (make-latency-threshold-rule threshold-nanos rate rule-name)
  (sample-rule
   rule-name
   (lambda (tr) (> (trace-record-duration tr) threshold-nanos))
   rate))

;; Rule: match if trace traverses a specific service
(define (make-service-match-rule service-name rate)
  (sample-rule
   (format "service-match-~a" service-name)
   (lambda (tr) (and (member service-name (trace-record-services tr)) #t))
   rate))

;; Rule: match if any span contains key matching expected value or regex
(define (make-attribute-match-rule attr-key attr-val rate rule-name)
  (sample-rule
   rule-name
   (lambda (tr)
     (for/or ([s (in-list (trace-record-spans tr))])
       (define v (span-get-attribute s attr-key))
       (cond
         [(not v) #f]
         [(equal? v attr-val) #t]
         [(and (string? v) (string? attr-val) (regexp-match? attr-val v)) #t]
         [else #f])))
   rate))

;; Fallback probabilistic background sampling
(define (make-probabilistic-rule rate)
  (sample-rule
   "background-probabilistic"
   (lambda (tr) #t)
   rate))

;; Constructs policy
(define (make-sampling-policy [rules '()] [default-rate 0.05])
  (sampling-policy rules default-rate))

;; Standard production policy:
;; - 100% of traces with errors
;; - 100% of traces exceeding 250ms
;; - 100% of traces hitting payment-gateway
;; - 5% baseline background traffic
(define default-production-policy
  (make-sampling-policy
   (list (make-error-rule 1.0)
         (make-latency-threshold-rule 250000000 1.0 "retain-slow-p99")
         (make-service-match-rule "payment-gateway" 1.0)
         (make-probabilistic-rule 0.05))
   0.05))
