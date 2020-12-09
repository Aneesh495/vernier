#lang racket/base

;;; tests/unit/test_sampler.rkt
;;; Unit test suite for tail-based sampling rules, in-memory trace buffers,
;;; evaluation decisions, and adaptive feedback rate controllers.

(require rackunit
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/sampler/rules.rkt"
         "../../vernier/sampler/tail_buffer.rkt"
         "../../vernier/sampler/evaluator.rkt"
         "../../vernier/sampler/adaptive.rkt")

(test-case "Sampling rule predicates"
  (define tid (make-trace-id))
  
  ;; Normal fast trace
  (define s-ok (make-span #:trace-id tid #:span-id (make-span-id)
                          #:name "GetCart" #:service-name "cart"
                          #:start-time 1000 #:end-time 2000 #:status-code 'ok))
  (define tr-ok (make-trace (list s-ok)))
  
  ;; Error trace
  (define s-err (make-span #:trace-id tid #:span-id (make-span-id)
                           #:name "Charge" #:service-name "payment-gateway"
                           #:start-time 1000 #:end-time 2000 #:status-code 'error))
  (define tr-err (make-trace (list s-err)))
  
  ;; Slow trace
  (define s-slow (make-span #:trace-id tid #:span-id (make-span-id)
                            #:name "Report" #:service-name "analytics"
                            #:start-time 1000 #:end-time (+ 1000 300000000) #:status-code 'ok))
  (define tr-slow (make-trace (list s-slow)))
  
  (define err-rule (make-error-rule 1.0))
  (check-false ((sample-rule-predicate err-rule) tr-ok))
  (check-true ((sample-rule-predicate err-rule) tr-err))
  
  (define slow-rule (make-latency-threshold-rule 250000000 1.0 "slow"))
  (check-false ((sample-rule-predicate slow-rule) tr-ok))
  (check-true ((sample-rule-predicate slow-rule) tr-slow)))

(test-case "Tail buffer span accumulation and reap"
  (define tb (make-tail-buffer 100 500000000 2000000000))
  (define tid (make-trace-id))
  (define t0 1000000000)
  
  (define root-span
    (make-span #:trace-id tid #:span-id (make-span-id)
               #:name "Root" #:service-name "web"
               #:start-time t0 #:end-time (+ t0 1000000)))
  
  (define tb1 (tail-buffer-insert-span tb root-span t0))
  (check-equal? (tail-buffer-count tb1) 1)
  
  ;; Immediately after arrival, trace is active and not reaped
  (define-values (tb-act reaped-empty) (tail-buffer-reap-completed tb1 (+ t0 100000000)))
  (check-equal? (length reaped-empty) 0)
  (check-equal? (tail-buffer-count tb-act) 1)
  
  ;; After inactivity timeout (600ms later), trace is reaped
  (define-values (tb-reaped reaped-traces) (tail-buffer-reap-completed tb1 (+ t0 600000000)))
  (check-equal? (length reaped-traces) 1)
  (check-equal? (tail-buffer-count tb-reaped) 0)
  (check-equal? (trace-record-trace-id (car reaped-traces)) tid))

(test-case "Sampling evaluator and batch decisions"
  (define policy
    (make-sampling-policy
     (list (make-error-rule 1.0)
           (make-latency-threshold-rule 200000000 1.0 "retain-slow"))
     0.0)) ;; 0% background rate for test determinism
  
  (define tid-err (make-trace-id))
  (define s-err (make-span #:trace-id tid-err #:span-id (make-span-id)
                           #:name "Err" #:service-name "A" #:start-time 0 #:end-time 100
                           #:status-code 'error))
  (define tr-err (make-trace (list s-err)))
  
  (define tid-norm (make-trace-id))
  (define s-norm (make-span #:trace-id tid-norm #:span-id (make-span-id)
                            #:name "Norm" #:service-name "A" #:start-time 0 #:end-time 100
                            #:status-code 'ok))
  (define tr-norm (make-trace (list s-norm)))
  
  (define dec-err (evaluate-trace-sampling policy tr-err))
  (check-true (sampling-decision-sampled? dec-err))
  (check-equal? (sampling-decision-rule-name dec-err) "retain-all-errors")
  (check-equal? (sampling-decision-sample-weight dec-err) 1.0)
  
  (define dec-norm (evaluate-trace-sampling policy tr-norm))
  (check-false (sampling-decision-sampled? dec-norm))
  
  (define-values (kept dropped) (evaluate-trace-batch policy (list tr-err tr-norm)))
  (check-equal? (length kept) 1)
  (check-equal? (length dropped) 1))

(test-case "Adaptive rate controller"
  (define ctrl (make-adaptive-controller 100.0 0.50 0.50))
  ;; Huge traffic spike arrives: 1000 traces in 1 second
  (define tuned (adaptive-record-throughput ctrl 1000 (+ (current-seconds) 1)))
  (check-true (< (adaptive-controller-current-rate tuned) 0.50)
              "High arrival rate must reduce background sample rate")
  
  ;; Low traffic arrives: 10 traces in 1 second
  (define tuned-low (adaptive-record-throughput ctrl 10 (+ (current-seconds) 2)))
  (check-true (> (adaptive-controller-current-rate tuned-low) 0.50)
              "Low arrival rate must increase background sample rate"))
