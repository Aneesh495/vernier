#lang racket/base

;;; vernier/dag/clock_skew.rkt
;;; Cross-host clock skew compensation using convex hull causality intervals.
;;; Rectifies timestamp drift between independent microservice nodes.

(require racket/contract
         racket/list
         racket/match
         "../core/span.rkt"
         "../core/trace.rkt"
         "graph.rkt")

(provide
 (contract-out
  ;; Clock offset estimation per service pair
  [struct service-skew-offset ([service-a string?]
                               [service-b string?]
                               [offset-nanos exact-integer?]
                               [confidence real?])]
  
  [estimate-clock-skew (-> trace-dag? (listof service-skew-offset?))]
  [compensate-trace-skew (-> trace-record? trace-record?)]
  [adjust-span-skew (-> span? exact-integer? span?)]))

(struct service-skew-offset (service-a service-b offset-nanos confidence)
  #:transparent)

;; Adjusts an individual span's timestamps by a given nanosecond offset
(define (adjust-span-skew s offset)
  (if (zero? offset)
      s
      (let* ([new-start (+ (span-start-time s) offset)]
             [new-end (+ (span-end-time s) offset)]
             [new-events (map (lambda (e)
                                (struct-copy span-event e
                                             [timestamp (+ (span-event-timestamp e) offset)]))
                              (span-events s))])
        (struct-copy span s
                     [start-time new-start]
                     [end-time new-end]
                     [events new-events]))))

;; Estimates clock skew between all caller-callee service pairs in the DAG
(define (estimate-clock-skew g)
  (define service-bounds (make-hash)) ;; (cons svcA svcB) -> list of (cons min-offset max-offset)
  
  (for ([(sid parent-span) (in-hash (trace-dag-nodes g))])
    (define children-ids (hash-ref (trace-dag-adj-out g) sid '()))
    (for ([cid (in-list children-ids)])
      (define child-span (hash-ref (trace-dag-nodes g) cid))
      (define svcA (span-service-name parent-span))
      (define svcB (span-service-name child-span))
      
      ;; Only calculate cross-service skew
      (unless (string=? svcA svcB)
        (define pair-key (cons svcA svcB))
        ;; Causal equations:
        ;; start(C) >= start(P)  ==> offset <= start(C) - start(P)
        ;; end(C) <= end(P)      ==> offset >= end(C) - end(P)
        (define max-offset (- (span-start-time child-span) (span-start-time parent-span)))
        (define min-offset (- (span-end-time child-span) (span-end-time parent-span)))
        (hash-update! service-bounds pair-key
                      (lambda (lst) (cons (cons min-offset max-offset) lst))
                      '()))))
  
  ;; Solve for optimal offset per service pair using interval consensus
  (for/list ([(pair bounds) (in-hash service-bounds)])
    (define svcA (car pair))
    (define svcB (cdr pair))
    (define min-bound (apply max (map car bounds)))
    (define max-bound (apply min (map cdr bounds)))
    
    (define-values (chosen-offset conf)
      (if (<= min-bound max-bound)
          ;; Feasible region exists
          (values (quotient (+ min-bound max-bound) 2) 1.0)
          ;; Inconsistent due to latency anomalies; take average violation
          (values (quotient (+ min-bound max-bound) 2) 0.5)))
    
    (service-skew-offset svcA svcB chosen-offset conf)))

;; Adjusts all spans in a trace record to restore physical causality
(define (compensate-trace-skew tr)
  (define g (trace->dag tr))
  (define root (trace-record-root-span tr))
  (if (or (not root) (<= (length (trace-record-spans tr)) 1))
      tr
      (let* ([root-svc (span-service-name root)]
             [service-offsets (make-hash)]
             [skew-estimates (estimate-clock-skew g)])
        ;; Root service has zero offset (anchor clock)
        (hash-set! service-offsets root-svc 0)
        
        ;; Propagate offsets through caller-callee relationships
        (for ([est (in-list skew-estimates)])
          (define svcA (service-skew-offset-service-a est))
          (define svcB (service-skew-offset-service-b est))
          (define off (service-skew-offset-offset-nanos est))
          (when (and (hash-has-key? service-offsets svcA)
                     (not (hash-has-key? service-offsets svcB)))
            ;; offset(B) = offset(A) + off
            (hash-set! service-offsets svcB (+ (hash-ref service-offsets svcA) off))))
        
        ;; Apply corrections to spans
        (define adjusted-spans
          (for/list ([s (in-list (trace-record-spans tr))])
            (define svc (span-service-name s))
            (define offset (hash-ref service-offsets svc 0))
            ;; Only adjust if offset is significantly nonzero (> 1 microsecond)
            (if (> (abs offset) 1000)
                (adjust-span-skew s (- offset))
                s)))
        
        (make-trace adjusted-spans))))
