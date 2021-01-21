#lang racket/base

;;; tests/unit/test_profiler.rkt
;;; Unit test suite for waterfall timelines, flamegraph trees, and topology graphs.

(require rackunit
         racket/string
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/profiler/waterfall.rkt"
         "../../vernier/profiler/flamegraph.rkt"
         "../../vernier/profiler/topology.rkt")

(test-case "Waterfall layout computation and ASCII rendering"
  (define tid (make-trace-id))
  (define s1 (make-span #:trace-id tid #:span-id "0000000000000001" #:name "GET /api"
                        #:service-name "gateway" #:start-time 1000 #:end-time 2000))
  (define s2 (make-span #:trace-id tid #:span-id "0000000000000002" #:parent-id "0000000000000001"
                        #:name "Auth" #:service-name "auth" #:start-time 1100 #:end-time 1400))
  (define tr (make-trace (list s1 s2)))
  
  (define rows (build-waterfall-layout tr))
  (check-equal? (length rows) 2)
  (check-equal? (waterfall-row-depth (car rows)) 0)
  (check-equal? (waterfall-row-depth (cadr rows)) 1)
  
  (define ascii (render-waterfall-ascii tr))
  (check-pred string? ascii)
  (check-true (> (string-length ascii) 50)))

(test-case "Flamegraph tree and collapsed stack generation"
  (define tid (make-trace-id))
  (define s1 (make-span #:trace-id tid #:span-id "0000000000000001" #:name "Root"
                        #:service-name "A" #:start-time 0 #:end-time 1000))
  (define s2 (make-span #:trace-id tid #:span-id "0000000000000002" #:parent-id "0000000000000001"
                        #:name "Child" #:service-name "B" #:start-time 100 #:end-time 800))
  (define tr (make-trace (list s1 s2)))
  
  (define tree (trace->flamegraph-tree tr))
  (check-equal? (flame-node-name tree) "Root")
  (check-equal? (length (flame-node-children tree)) 1)
  (check-equal? (flame-node-name (car (flame-node-children tree))) "Child")
  
  (define stacks (trace->collapsed-stacks tr))
  (check-true (> (length stacks) 0))
  
  (define svg (render-flamegraph-svg tr))
  (check-pred string? svg)
  (check-true (string-contains? svg "<svg")))

(test-case "Service dependency topology graph"
  (define tid (make-trace-id))
  (define s1 (make-span #:trace-id tid #:span-id "0000000000000001" #:name "Gateway"
                        #:service-name "api-gateway" #:start-time 1000 #:end-time 2000))
  (define s2 (make-span #:trace-id tid #:span-id "0000000000000002" #:parent-id "0000000000000001"
                        #:name "DB" #:service-name "postgres" #:start-time 1200 #:end-time 1800
                        #:status-code 'ok))
  (define tr (make-trace (list s1 s2)))
  
  (define topo (build-service-topology (list tr)))
  (check-equal? (length (service-topology-nodes topo)) 2)
  (check-equal? (length (service-topology-edges topo)) 1)
  
  (define edge (car (service-topology-edges topo)))
  (check-equal? (topology-edge-source-service edge) "api-gateway")
  (check-equal? (topology-edge-target-service edge) "postgres")
  (check-equal? (topology-edge-call-count edge) 1)
  
  (define topo-dict (topology->hash topo))
  (check-pred hash? topo-dict))
