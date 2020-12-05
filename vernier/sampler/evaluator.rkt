#lang racket/base

;;; vernier/sampler/evaluator.rkt
;;; Sampling policy evaluator and sample weight calculator.
;;; Executes deterministic or probabilistic sampling decisions on completed traces.

(require racket/contract
         racket/list
         racket/random
         "../core/trace.rkt"
         "rules.rkt")

(provide
 (contract-out
  ;; Sampling outcome record
  [struct sampling-decision ([sampled? boolean?]
                             [rule-name string?]
                             [sample-rate real?]
                             [sample-weight real?])]
  
  [evaluate-trace-sampling (-> sampling-policy? trace-record? sampling-decision?)]
  [evaluate-trace-batch (-> sampling-policy? (listof trace-record?)
                            (values (listof (cons/c trace-record? sampling-decision?))
                                    (listof trace-record?)))]))

(struct sampling-decision (sampled? rule-name sample-rate sample-weight)
  #:transparent)

;; Evaluates trace against ordered rules in the sampling policy
(define (evaluate-trace-sampling policy tr)
  (define matched-rule
    (for/or ([rule (in-list (sampling-policy-rules policy))])
      (and ((sample-rule-predicate rule) tr)
           rule)))
  
  (define-values (r-name rate)
    (if matched-rule
        (values (sample-rule-name matched-rule) (sample-rule-sample-rate matched-rule))
        (values "default-fallback" (sampling-policy-default-rate policy))))
  
  ;; Probabilistic draw: sample if random float < rate
  (define sampled?
    (cond
      [(>= rate 1.0) #t]
      [(<= rate 0.0) #f]
      [else (< (random) rate)]))
  
  (define weight (if (zero? rate) 1.0 (/ 1.0 rate)))
  (sampling-decision sampled? r-name rate weight))

;; Evaluates batch of traces, splitting into kept and dropped lists
(define (evaluate-trace-batch policy traces)
  (define-values (kept dropped)
    (for/fold ([k '()] [d '()])
              ([tr (in-list traces)])
      (define dec (evaluate-trace-sampling policy tr))
      (if (sampling-decision-sampled? dec)
          (values (cons (cons tr dec) k) d)
          (values k (cons tr d)))))
  (values (reverse kept) (reverse dropped)))
