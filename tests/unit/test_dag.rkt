#lang racket/base

;;; tests/unit/test_dag.rkt
;;; Unit test suite for DAG construction, cycle breaking, clock skew,
;;; critical path analysis, and structural trace differencing.

(require rackunit
         racket/set
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/dag/graph.rkt"
         "../../vernier/dag/cycle.rkt"
         "../../vernier/dag/clock_skew.rkt"
         "../../vernier/dag/critical_path.rkt"
         "../../vernier/dag/diff.rkt")

(test-case "DAG construction and topological sorting"
  (define tid (make-trace-id))
  (define s1 (make-span #:trace-id tid #:span-id "0000000000000001" #:name "Root"
                        #:service-name "A" #:start-time 100 #:end-time 500))
  (define s2 (make-span #:trace-id tid #:span-id "0000000000000002" #:parent-id "0000000000000001"
                        #:name "Child1" #:service-name "B" #:start-time 150 #:end-time 300))
  (define s3 (make-span #:trace-id tid #:span-id "0000000000000003" #:parent-id "0000000000000001"
                        #:name "Child2" #:service-name "C" #:start-time 310 #:end-time 450))
  
  (define tr (make-trace (list s1 s2 s3)))
  (define g (trace->dag tr))
  (check-equal? (length (trace-dag-roots g)) 1)
  (check-equal? (car (trace-dag-roots g)) "0000000000000001")
  (check-equal? (length (trace-dag-leaves g)) 2)
  (check-equal? (dag-depth g "0000000000000001") 0)
  (check-equal? (dag-depth g "0000000000000002") 1)
  (check-equal? (dag-max-depth g) 1)
  
  (define topo (dag-topological-sort g))
  (check-not-false topo)
  (check-equal? (car topo) "0000000000000001")
  (check-false (dag-cyclic? g)))

(test-case "Cycle detection and cycle breaker"
  ;; Create a cycle: 1 -> 2 -> 3 -> 1
  (define tid (make-trace-id))
  (define s1 (make-span #:trace-id tid #:span-id "0000000000000001" #:parent-id "0000000000000003"
                        #:name "A" #:service-name "svc" #:start-time 100 #:end-time 200))
  (define s2 (make-span #:trace-id tid #:span-id "0000000000000002" #:parent-id "0000000000000001"
                        #:name "B" #:service-name "svc" #:start-time 200 #:end-time 300))
  (define s3 (make-span #:trace-id tid #:span-id "0000000000000003" #:parent-id "0000000000000002"
                        #:name "C" #:service-name "svc" #:start-time 300 #:end-time 400))
  
  (define tr (make-trace (list s1 s2 s3)))
  (define g (trace->dag tr))
  (check-true (dag-cyclic? g))
  (check-false (dag-topological-sort g))
  
  (define break-res (dag-break-cycles g))
  (check-false (dag-cyclic? (cycle-break-result-sanitized-dag break-res)))
  (check-true (> (length (cycle-break-result-broken-edges break-res)) 0)))

(test-case "Clock skew estimation and compensation"
  (define tid (make-trace-id))
  ;; Client on Service A: [1000, 2000]
  ;; Server on Service B with +500ns clock offset: [1600, 2400]
  (define s-client (make-span #:trace-id tid #:span-id "0000000000000001" #:name "call"
                              #:service-name "frontend" #:start-time 1000 #:end-time 2000))
  (define s-server (make-span #:trace-id tid #:span-id "0000000000000002" #:parent-id "0000000000000001"
                              #:name "handle" #:service-name "backend" #:start-time 1600 #:end-time 2400))
  
  (define tr-skewed (make-trace (list s-client s-server)))
  (define g-skewed (trace->dag tr-skewed))
  (define skew-estimates (estimate-clock-skew g-skewed))
  (check-equal? (length skew-estimates) 1)
  (define est (car skew-estimates))
  (check-equal? (service-skew-offset-service-a est) "frontend")
  (check-equal? (service-skew-offset-service-b est) "backend")
  (check-true (> (service-skew-offset-offset-nanos est) 0))
  
  ;; Apply compensation
  (define tr-fixed (compensate-trace-skew tr-skewed))
  (check-not-false tr-fixed))

(test-case "Critical path analysis with concurrent asynchronous branches"
  ;; Root span runs from 1000 to 2000 (total duration 1000).
  ;; It launches two parallel RPCs:
  ;; Child 1 (fast): 1100 to 1300 (duration 200)
  ;; Child 2 (slow, bottleneck): 1100 to 1900 (duration 800)
  (define tid (make-trace-id))
  (define root-s (make-span #:trace-id tid #:span-id "0000000000000001" #:name "RootReq"
                            #:service-name "gateway" #:start-time 1000 #:end-time 2000))
  (define child-fast (make-span #:trace-id tid #:span-id "0000000000000002" #:parent-id "0000000000000001"
                                #:name "FastCache" #:service-name "redis" #:start-time 1100 #:end-time 1300))
  (define child-slow (make-span #:trace-id tid #:span-id "0000000000000003" #:parent-id "0000000000000001"
                                #:name "SlowDB" #:service-name "postgres" #:start-time 1100 #:end-time 1900))
  
  (define tr (make-trace (list root-s child-fast child-slow)))
  (define cp-res (analyze-critical-path tr))
  (check-equal? (critical-path-result-trace-id cp-res) tid)
  (check-equal? (critical-path-result-total-trace-duration cp-res) 1000)
  
  ;; The critical spans must contain the Root and Child 2 (SlowDB), NOT Child 1
  (define crit-ids (map span-span-id (critical-path-result-critical-spans cp-res)))
  (check-not-false (member "0000000000000001" crit-ids) "Root must be on critical path")
  (check-not-false (member "0000000000000003" crit-ids) "SlowDB must be on critical path")
  (check-false (member "0000000000000002" crit-ids) "FastCache must not be on critical path")
  
  ;; Verify service attributions: postgres should dominate
  (define attrs (critical-path-result-attributions cp-res))
  (define pg-attr (findf (lambda (a) (string=? (service-attribution-service-name a) "postgres")) attrs))
  (check-not-false pg-attr)
  (check-true (> (service-attribution-percentage pg-attr) 50.0)))

(test-case "Structural trace diffing"
  (define tid-a (make-trace-id))
  (define tid-b (make-trace-id))
  
  (define s1-a (make-span #:trace-id tid-a #:span-id "0000000000000001" #:name "API" #:service-name "web"
                          #:start-time 100 #:end-time 200))
  (define s2-a (make-span #:trace-id tid-a #:span-id "0000000000000002" #:parent-id "0000000000000001"
                          #:name "Query" #:service-name "db" #:start-time 120 #:end-time 180))
  
  (define s1-b (make-span #:trace-id tid-b #:span-id "0000000000000001" #:name "API" #:service-name "web"
                          #:start-time 100 #:end-time 350))
  (define s2-b (make-span #:trace-id tid-b #:span-id "0000000000000002" #:parent-id "0000000000000001"
                          #:name "Query" #:service-name "db" #:start-time 120 #:end-time 280))
  (define s3-b (make-span #:trace-id tid-b #:span-id "0000000000000003" #:parent-id "0000000000000001"
                          #:name "Retry" #:service-name "db" #:start-time 290 #:end-time 340))
  
  (define tr-a (make-trace (list s1-a s2-a)))
  (define tr-b (make-trace (list s1-b s2-b s3-b)))
  
  (define d (diff-traces tr-a tr-b))
  (check-equal? (trace-diff-result-trace-id-a d) tid-a)
  (check-equal? (trace-diff-result-trace-id-b d) tid-b)
  (check-equal? (length (trace-diff-result-matched-spans d)) 2)
  (check-equal? (length (trace-diff-result-added-in-b d)) 1)
  (check-equal? (length (trace-diff-result-removed-in-b d)) 0)
  (check-true (> (trace-diff-result-total-duration-delta d) 0)))
