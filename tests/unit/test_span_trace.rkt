#lang racket/base

;;; tests/unit/test_span_trace.rkt
;;; Unit test suite for span data structures, trace indexing, and structural contracts.

(require rackunit
         "../../vernier/core/id.rkt"
         "../../vernier/core/time.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/core/contract.rkt")

(test-case "Span creation and accessors"
  (define tid (make-trace-id))
  (define sid (make-span-id))
  (define t0 1000000000)
  (define t1 1250000000)
  
  (define s (make-span #:trace-id tid
                       #:span-id sid
                       #:name "GET /api/v1/orders"
                       #:service-name "gateway"
                       #:kind 'server
                       #:start-time t0
                       #:end-time t1
                       #:status-code 'ok
                       #:attributes (hasheq 'http.method "GET" 'http.status 200)))
  
  (check-equal? (span-trace-id s) tid)
  (check-equal? (span-span-id s) sid)
  (check-false (span-parent-id s))
  (check-true (span-root? s))
  (check-true (span-server? s))
  (check-false (span-client? s))
  (check-false (span-error? s))
  (check-equal? (span-duration s) 250000000)
  (check-equal? (span-get-attribute s 'http.method) "GET")
  (check-equal? (span-get-attribute s 'nonexistent "default") "default"))

(test-case "Span functional modification and serialization round-trip"
  (define tid (make-trace-id))
  (define sid (make-span-id))
  (define s0 (make-span #:trace-id tid
                        #:span-id sid
                        #:name "compute"
                        #:service-name "worker"
                        #:start-time 100
                        #:end-time 200))
  
  (define s1 (span-set-attribute s0 'thread.id 42))
  (check-equal? (span-get-attribute s1 'thread.id) 42)
  (check-false (span-get-attribute s0 'thread.id))
  
  (define evt (make-span-event "cache-miss" 150 (hasheq 'key "item-99")))
  (define s2 (span-add-event s1 evt))
  (check-equal? (length (span-events s2)) 1)
  (check-equal? (span-event-name (car (span-events s2))) "cache-miss")
  
  (define s-err (span-set-status s2 'error "database timeout"))
  (check-true (span-error? s-err))
  (check-equal? (span-status-message s-err) "database timeout")
  
  ;; Serialization round-trip
  (define h (span->hash s-err))
  (check-pred hash? h)
  (define restored (hash->span h))
  (check-equal? (span-trace-id restored) (span-trace-id s-err))
  (check-equal? (span-span-id restored) (span-span-id s-err))
  (check-equal? (span-name restored) (span-name s-err))
  (check-equal? (span-duration restored) (span-duration s-err))
  (check-equal? (span-status-code restored) 'error)
  (check-equal? (length (span-events restored)) 1))

(test-case "Trace collection, indexing, and root discovery"
  (define tid (make-trace-id))
  (define root-id (make-span-id))
  (define child1-id (make-span-id))
  (define child2-id (make-span-id))
  
  (define root-span
    (make-span #:trace-id tid
               #:span-id root-id
               #:name "HTTP GET /checkout"
               #:service-name "web"
               #:kind 'server
               #:start-time 1000
               #:end-time 2000
               #:status-code 'ok))
  
  (define child1
    (make-span #:trace-id tid
               #:span-id child1-id
               #:parent-id root-id
               #:name "Auth Check"
               #:service-name "auth"
               #:kind 'client
               #:start-time 1100
               #:end-time 1300
               #:status-code 'ok))
  
  (define child2
    (make-span #:trace-id tid
               #:span-id child2-id
               #:parent-id root-id
               #:name "DB Query"
               #:service-name "postgres"
               #:kind 'client
               #:start-time 1400
               #:end-time 1900
               #:status-code 'error
               #:status-message "deadlock detected"))
  
  (define tr (make-trace (list root-span child1 child2)))
  (check-not-false tr)
  (check-equal? (trace-record-trace-id tr) tid)
  (check-equal? (length (trace-record-spans tr)) 3)
  (check-equal? (trace-record-start-time tr) 1000)
  (check-equal? (trace-record-end-time tr) 2000)
  (check-equal? (trace-record-duration tr) 1000)
  (check-true (trace-record-complete? tr))
  (check-true (trace-has-errors? tr))
  (check-equal? (trace-record-error-count tr) 1)
  
  ;; Lookup tests
  (check-equal? (span-name (trace-find-span tr child1-id)) "Auth Check")
  (check-false (trace-find-span tr "nonexistent"))
  
  ;; Children lookup
  (define root-children (trace-find-children tr root-id))
  (check-equal? (length root-children) 2)
  (check-equal? (length (trace-find-children tr child1-id)) 0)
  
  ;; Summary
  (define summary (trace->summary tr))
  (check-equal? (trace-summary-root-name summary) "HTTP GET /checkout")
  (check-equal? (trace-summary-root-service summary) "web")
  (check-equal? (trace-summary-span-count summary) 3)
  (check-equal? (trace-summary-error-count summary) 1))

(test-case "Trace integrity and contract validation"
  (define tid (make-trace-id))
  (define sid1 (make-span-id))
  (define sid2 (make-span-id))
  
  ;; Valid parent-child pair
  (define s1 (make-span #:trace-id tid #:span-id sid1 #:name "P" #:service-name "A"
                        #:start-time 1000 #:end-time 2000))
  (define s2 (make-span #:trace-id tid #:span-id sid2 #:parent-id sid1 #:name "C" #:service-name "B"
                        #:start-time 1100 #:end-time 1500))
  (define tr-valid (make-trace (list s1 s2)))
  (check-true (trace-strictly-valid? tr-valid))
  (check-equal? (length (verify-trace-integrity tr-valid)) 0)
  
  ;; Time inversion warning
  (define s-bad (make-span #:trace-id tid #:span-id sid2 #:parent-id sid1 #:name "C" #:service-name "B"
                           #:start-time 900 #:end-time 1500))
  (define tr-skewed (make-trace (list s1 s-bad)))
  (define vios (verify-trace-integrity tr-skewed))
  (check-true (> (length vios) 0))
  (check-equal? (integrity-violation-code (car vios)) 'time-inversion-start))
