#lang racket/base

;;; vernier/profiler/topology.rkt
;;; Service dependency topology graph builder and communication metrics.
;;; Discovers microservice dependencies and inter-service call volumes.

(require racket/contract
         racket/list
         racket/set
         "../core/span.rkt"
         "../core/trace.rkt"
         "../dag/graph.rkt"
         "../sketch/quantile.rkt")

(provide
 (contract-out
  ;; Topology graph structures
  [struct topology-edge ([source-service string?]
                         [target-service string?]
                         [call-count exact-nonnegative-integer?]
                         [error-count exact-nonnegative-integer?]
                         [p95-nanos (or/c real? #f)])]
  
  [struct topology-node ([service-name string?]
                         [total-calls exact-nonnegative-integer?]
                         [error-count exact-nonnegative-integer?]
                         [error-rate real?]
                         [health-status (or/c 'healthy 'degraded 'failing)])]
  
  [struct service-topology ([nodes (listof topology-node?)]
                            [edges (listof topology-edge?)])]
  
  [build-service-topology (-> (listof trace-record?) service-topology?)]
  [topology->hash (-> service-topology? hash?)]))

(struct topology-edge (source-service target-service call-count error-count p95-nanos)
  #:transparent)

(struct topology-node (service-name total-calls error-count error-rate health-status)
  #:transparent)

(struct service-topology (nodes edges)
  #:transparent)

;; Builds service topology from a collection of traces
(define (build-service-topology traces)
  (define node-calls (make-hash))
  (define node-errors (make-hash))
  
  ;; Map (cons source target) -> (list call-count error-count dd-sketch)
  (define edge-data (make-hash))
  
  (for ([tr (in-list traces)])
    (define g (trace->dag tr))
    (for ([(sid span) (in-hash (trace-dag-nodes g))])
      (define svc (span-service-name span))
      (hash-update! node-calls svc add1 0)
      (when (span-error? span)
        (hash-update! node-errors svc add1 0))
      
      ;; Inspect caller-callee relationships
      (define child-ids (hash-ref (trace-dag-adj-out g) sid '()))
      (for ([cid (in-list child-ids)])
        (define child-span (hash-ref (trace-dag-nodes g) cid))
        (define child-svc (span-service-name child-span))
        (unless (string=? svc child-svc)
          (define edge-key (cons svc child-svc))
          (define existing
            (hash-ref edge-data edge-key
                      (lambda () (list 0 0 (make-dd-sketch 0.02)))))
          (define new-count (add1 (list-ref existing 0)))
          (define new-err (if (span-error? child-span) (add1 (list-ref existing 1)) (list-ref existing 1)))
          (define new-sk (dd-sketch-insert (list-ref existing 2) (span-duration child-span)))
          (hash-set! edge-data edge-key (list new-count new-err new-sk))))))
  
  ;; Construct nodes
  (define nodes
    (for/list ([svc (in-list (sort (hash-keys node-calls) string<?))])
      (define calls (hash-ref node-calls svc 0))
      (define errs (hash-ref node-errors svc 0))
      (define err-rate (if (zero? calls) 0.0 (/ errs (* 1.0 calls))))
      (define health
        (cond
          [(>= err-rate 0.10) 'failing]
          [(> err-rate 0.01) 'degraded]
          [else 'healthy]))
      (topology-node svc calls errs err-rate health)))
  
  ;; Construct edges
  (define edges
    (for/list ([(pair data) (in-hash edge-data)])
      (define calls (list-ref data 0))
      (define errs (list-ref data 1))
      (define sk (list-ref data 2))
      (define p95 (dd-sketch-quantile sk 0.95))
      (topology-edge (car pair) (cdr pair) calls errs p95)))
  
  (service-topology nodes edges))

;; Serializes topology to dictionary for JSON output
(define (topology->hash topo)
  (hash
   'nodes
   (map (lambda (n)
          (hash 'service (topology-node-service-name n)
                'total_calls (topology-node-total-calls n)
                'error_count (topology-node-error-count n)
                'error_rate (topology-node-error-rate n)
                'health (symbol->string (topology-node-health-status n))))
        (service-topology-nodes topo))
   'edges
   (map (lambda (e)
          (hash 'source (topology-edge-source-service e)
                'target (topology-edge-target-service e)
                'call_count (topology-edge-call-count e)
                'error_count (topology-edge-error-count e)
                'p95_latency_nanos (or (topology-edge-p95-nanos e) 0)))
        (service-topology-edges topo))))
