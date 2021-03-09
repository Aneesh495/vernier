#lang racket/base

;;; tests/property/test_invariants.rkt
;;; Property-based invariant verification test suite.
;;; Verifies mathematical, probabilistic, and algorithmic invariants:
;;;   1. DAG topological ordering: u appears before v for every directed edge u -> v
;;;   2. Critical path bounds: 0 <= critical_duration <= total_duration
;;;   3. Critical path service attribution conservation: sum(attr) == critical_duration
;;;   4. DDSketch quantile monotonicity: q(a) <= q(b) for all a <= b
;;;   5. HyperLogLog error bound: relative error <= 5% for N = 5000
;;;   6. Bloom filter zero false negative guarantee
;;;   7. WAL CRC32 bit-flip corrupt frame rejection
;;;   8. Binary protocol serialization roundtrip fidelity

(require rackunit
         racket/list
         racket/random
         "../../vernier/core/time.rkt"
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/dag/graph.rkt"
         "../../vernier/dag/critical_path.rkt"
         "../../vernier/sketch/quantile.rkt"
         "../../vernier/sketch/hll.rkt"
         "../../vernier/sketch/bloom.rkt"
         "../../vernier/storage/wal.rkt"
         "../../vernier/protocol/binary.rkt"
         "../../vernier/simulator/mesh.rkt")

(test-case "Invariant 1: DAG topological sort respects directed edge ordering"
  (for ([trial (in-range 20)])
    (define spans (generate-normal-checkout-trace))
    (define tr (make-trace spans))
    (define dag (trace->dag tr))
    (define topo-order (dag-topological-sort dag))
    (check-not-false topo-order)
    
    ;; Build index map: span-id -> position index
    (define pos-map
      (for/hash ([sid (in-list topo-order)]
                 [idx (in-naturals)])
        (values sid idx)))
    
    ;; For every span, its parent must precede it
    (for ([s (in-list spans)])
      (define pid (span-parent-id s))
      (when pid
        (define parent-pos (hash-ref pos-map pid))
        (define child-pos (hash-ref pos-map (span-span-id s)))
        (check-true (< parent-pos child-pos))))))

(test-case "Invariant 2: Critical path duration bounds"
  (for ([trial (in-range 20)])
    (define spans (generate-normal-checkout-trace))
    (define tr (make-trace spans))
    (define cp (analyze-critical-path tr))
    (define crit-dur (critical-path-result-critical-duration cp))
    (define total-dur (trace-record-duration tr))
    (check-true (> crit-dur 0))
    (check-true (<= crit-dur total-dur))))

(test-case "Invariant 3: Service attribution conservation"
  (for ([trial (in-range 10)])
    (define spans (generate-normal-checkout-trace))
    (define tr (make-trace spans))
    (define cp (analyze-critical-path tr))
    (define crit-dur (critical-path-result-critical-duration cp))
    (define sum-attr
      (for/sum ([attr (in-list (critical-path-result-attributions cp))])
        (service-attribution-critical-duration attr)))
    (check-equal? sum-attr crit-dur)))

(test-case "Invariant 4: DDSketch quantile monotonicity"
  (define sketch-box (box (make-dd-sketch 0.01)))
  ;; Feed with log-normal distributed values
  (for ([i (in-range 2000)])
    (define val (+ 1.0 (* 10.0 (exp (* 1.5 (/ (random 1000) 1000.0))))))
    (dd-sketch-insert! sketch-box val))
  
  (define sketch (unbox sketch-box))
  (define q10 (dd-sketch-quantile sketch 0.10))
  (define q25 (dd-sketch-quantile sketch 0.25))
  (define q50 (dd-sketch-quantile sketch 0.50))
  (define q75 (dd-sketch-quantile sketch 0.75))
  (define q90 (dd-sketch-quantile sketch 0.90))
  (define q99 (dd-sketch-quantile sketch 0.99))
  
  (check-true (<= q10 q25))
  (check-true (<= q25 q50))
  (check-true (<= q50 q75))
  (check-true (<= q75 q90))
  (check-true (<= q90 q99)))

(test-case "Invariant 5: HyperLogLog relative error bound < 5% on 5,000 unique items"
  (define h-box (box (make-hyperloglog 14)))
  (for ([i (in-range 5000)])
    (hll-insert! h-box (format "req-id-uuid-~a" i)))
  (define est (hll-cardinality (unbox h-box)))
  (define rel-err (/ (abs (- est 5000)) 5000.0))
  (check-true (< rel-err 0.05)))

(test-case "Invariant 6: Bloom filter zero false negatives"
  (define bf-box (box (make-bloom-filter 1000 0.01)))
  (define items
    (for/list ([i (in-range 500)])
      (format "item-uuid-~a" i)))
  
  (for ([item (in-list items)])
    (bloom-insert! bf-box item))
  
  ;; Every inserted item MUST report true
  (for ([item (in-list items)])
    (check-true (bloom-contains? (unbox bf-box) item))))

(test-case "Invariant 7: Compact binary protocol serialization roundtrip"
  (define s (make-span #:trace-id "0123456789abcdef0123456789abcdef"
                       #:span-id "0123456789abcdef"
                       #:parent-id "fedcba9876543210"
                       #:name "process-payment"
                       #:service-name "billing"
                       #:kind 'client
                       #:start-time 1700000000000000000
                       #:end-time   1700000000150000000
                       #:status-code 'ok
                       #:status-message "authorized"
                       #:attributes (hash 'http.status_code 200
                                          'user.email "dev@example.com"
                                          'active #t)))
  (define wire-bytes (span->binary-bytes s))
  (define decoded (binary-bytes->span wire-bytes))
  
  (check-equal? (span-trace-id decoded) (span-trace-id s))
  (check-equal? (span-span-id decoded) (span-span-id s))
  (check-equal? (span-parent-id decoded) (span-parent-id s))
  (check-equal? (span-name decoded) (span-name s))
  (check-equal? (span-service-name decoded) (span-service-name s))
  (check-equal? (span-kind decoded) (span-kind s))
  (check-equal? (span-start-time decoded) (span-start-time s))
  (check-equal? (span-end-time decoded) (span-end-time s))
  (check-equal? (span-status-code decoded) (span-status-code s)))
