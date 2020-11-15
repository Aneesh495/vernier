#lang racket/base

;;; vernier/anomaly/pattern.rkt
;;; Structural anti-pattern detection across distributed trace DAGs.
;;; Detects N+1 query loops, fan-out explosions, retry storms, and deep chains.

(require racket/contract
         racket/list
         racket/set
         "../core/span.rkt"
         "../core/trace.rkt"
         "../dag/graph.rkt")

(provide
 (contract-out
  ;; Anti-pattern violation record
  [struct pattern-anomaly ([kind (or/c 'n-plus-one 'fan-out-explosion 'retry-storm 'deep-call-chain)]
                           [parent-span-id (or/c string? #f)]
                           [service-name string?]
                           [affected-count exact-nonnegative-integer?]
                           [wasted-duration-nanos exact-integer?]
                           [description string?]
                           [recommendation string?])]
  
  [detect-structural-anomalies (-> trace-record? (listof pattern-anomaly?))]))

(struct pattern-anomaly (kind
                         parent-span-id
                         service-name
                         affected-count
                         wasted-duration-nanos
                         description
                         recommendation)
  #:transparent)

;; Scans trace for architectural and performance anti-patterns
(define (detect-structural-anomalies tr)
  (define g (trace->dag tr))
  (append (detect-n-plus-one g)
          (detect-fan-out-explosions g)
          (detect-retry-storms g)
          (detect-deep-chains g)))

;; Detects N+1 query pattern: parent issues many identical/templated child operations
(define (detect-n-plus-one g)
  (define anomalies '())
  (for ([(pid parent-span) (in-hash (trace-dag-nodes g))])
    (define child-ids (hash-ref (trace-dag-adj-out g) pid '()))
    (when (>= (length child-ids) 6)
      ;; Group child spans by (cons service name)
      (define groups (make-hash))
      (for ([cid (in-list child-ids)])
        (define child (hash-ref (trace-dag-nodes g) cid))
        (define key (cons (span-service-name child) (span-name child)))
        (hash-update! groups key (lambda (lst) (cons child lst)) '()))
      
      (for ([(key spans) (in-hash groups)])
        (when (>= (length spans) 6)
          (define total-child-dur
            (for/sum ([s (in-list spans)]) (span-duration s)))
          (define svc (car key))
          (define op (cdr key))
          (set! anomalies
                (cons (pattern-anomaly
                       'n-plus-one
                       pid
                       (span-service-name parent-span)
                       (length spans)
                       total-child-dur
                       (format "N+1 query pattern detected: ~a sequential calls to '~a' on service '~a'"
                               (length spans) op svc)
                       "Consolidate individual sequential RPCs into a single batch or multi-get query")
                      anomalies))))))
  anomalies)

;; Detects excessive concurrent child fan-out
(define (detect-fan-out-explosions g [threshold 30])
  (define anomalies '())
  (for ([(pid parent-span) (in-hash (trace-dag-nodes g))])
    (define child-ids (hash-ref (trace-dag-adj-out g) pid '()))
    (when (> (length child-ids) threshold)
      (set! anomalies
            (cons (pattern-anomaly
                   'fan-out-explosion
                   pid
                   (span-service-name parent-span)
                   (length child-ids)
                   (span-duration parent-span)
                   (format "Excessive fan-out: span dispatched ~a concurrent child spans" (length child-ids))
                   "Throttle concurrent fan-out using worker pools or pipeline streaming")
                  anomalies))))
  anomalies)

;; Detects rapid repeated retry attempts following errors
(define (detect-retry-storms g [threshold 3])
  (define anomalies '())
  (for ([(pid parent-span) (in-hash (trace-dag-nodes g))])
    (define child-ids (hash-ref (trace-dag-adj-out g) pid '()))
    (define error-children
      (filter (lambda (cid)
                (span-error? (hash-ref (trace-dag-nodes g) cid)))
              child-ids))
    (when (>= (length error-children) threshold)
      (define wasted-dur
        (for/sum ([cid (in-list error-children)])
          (span-duration (hash-ref (trace-dag-nodes g) cid))))
      (set! anomalies
            (cons (pattern-anomaly
                   'retry-storm
                   pid
                   (span-service-name parent-span)
                   (length error-children)
                   wasted-dur
                   (format "Retry storm: detected ~a consecutive failed attempts" (length error-children))
                   "Implement exponential backoff with jitter and install circuit breaker protection")
                  anomalies))))
  anomalies)

;; Detects excessively deep microservice call topologies
(define (detect-deep-chains g [depth-threshold 10])
  (define max-d (dag-max-depth g))
  (if (> max-d depth-threshold)
      (list (pattern-anomaly
             'deep-call-chain
             (and (not (null? (trace-dag-roots g))) (car (trace-dag-roots g)))
             "system"
             max-d
             0
             (format "Deep execution graph: call chain depth reaches ~a hops (exceeds threshold ~a)"
                     max-d depth-threshold)
             "Refactor monolithic call chains into event-driven pub-sub or direct routing"))
      '()))
