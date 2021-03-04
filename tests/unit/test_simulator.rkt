#lang racket/base

;;; tests/unit/test_simulator.rkt
;;; Unit test suite for microservice simulator and synthetic trace generators.

(require rackunit
         racket/list
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/dag/graph.rkt"
         "../../vernier/simulator/mesh.rkt")

(test-case "Normal checkout trace structure"
  (define spans (generate-normal-checkout-trace))
  (check-equal? (length spans) 9)
  (define tr (make-trace spans))
  (check-not-false tr)
  (check-false (trace-has-errors? tr))
  (check-equal? (length (trace-record-services tr)) 6)
  (define g (trace->dag tr))
  (check-not-false (dag-topological-sort g)))

(test-case "N+1 query regression trace generation"
  (define spans (generate-n-plus-one-trace #:item-count 20))
  (check-equal? (length spans) 22)
  (define tr (make-trace spans))
  (check-not-false tr)
  (define g (trace->dag tr))
  (check-not-false (dag-topological-sort g)))

(test-case "Retry storm trace generation"
  (define spans (generate-retry-storm-trace))
  (check-equal? (length spans) 5)
  (define tr (make-trace spans))
  (check-not-false tr)
  (check-true (trace-has-errors? tr))
  (check-equal? (trace-record-error-count tr) 5))

(test-case "Fanout explosion trace generation"
  (define spans (generate-fanout-explosion-trace #:fanout 30))
  (check-equal? (length spans) 32)
  (define tr (make-trace spans))
  (check-not-false tr)
  (define g (trace->dag tr))
  (check-not-false (dag-topological-sort g)))

(test-case "Batch traffic generation"
  (define batch (simulate-traffic 10))
  (check-equal? (length batch) 10)
  (for ([tr-spans (in-list batch)])
    (check-true (pair? tr-spans))
    (check-not-false (make-trace tr-spans))))
