#lang racket/base

;;; vernier/dag/critical_path.rkt
;;; Dynamic programming critical path analysis on distributed execution DAGs.
;;; Deconstructs concurrent asynchronous spans into exact bottleneck chains.

(require racket/contract
         racket/list
         racket/set
         racket/match
         "../core/time.rkt"
         "../core/span.rkt"
         "../core/trace.rkt"
         "graph.rkt")

(provide
 (contract-out
  ;; Critical path segment
  [struct critical-segment ([span-id string?]
                            [service-name string?]
                            [name string?]
                            [start-time exact-integer?]
                            [end-time exact-integer?]
                            [duration exact-integer?]
                            [segment-type (or/c 'compute 'wait)])]
  
  ;; Service bottleneck attribution
  [struct service-attribution ([service-name string?]
                               [critical-duration exact-integer?]
                               [percentage real?])]
  
  ;; Full critical path result
  [struct critical-path-result ([trace-id string?]
                                [critical-spans (listof span?)]
                                [critical-duration exact-integer?]
                                [total-trace-duration exact-integer?]
                                [segments (listof critical-segment?)]
                                [attributions (listof service-attribution?)])]
  
  ;; Main analysis entrypoint
  [analyze-critical-path (-> trace-record? critical-path-result?)]))

(struct critical-segment (span-id service-name name start-time end-time duration segment-type)
  #:transparent)

(struct service-attribution (service-name critical-duration percentage)
  #:transparent)

(struct critical-path-result (trace-id
                              critical-spans
                              critical-duration
                              total-trace-duration
                              segments
                              attributions)
  #:transparent)

;; Analyzes the causal execution graph and determines the critical path
(define (analyze-critical-path tr)
  (define g (trace->dag tr))
  (define tid (trace-record-trace-id tr))
  (define root (trace-record-root-span tr))
  (define total-dur (max 1 (trace-record-duration tr)))
  
  (if (not root)
      (critical-path-result tid '() 0 total-dur '() '())
      (let* ([critical-sid-set (compute-critical-span-ids g root)]
             [critical-span-list
              (sort (filter (lambda (s) (set-member? critical-sid-set (span-span-id s)))
                            (trace-record-spans tr))
                    < #:key span-start-time)]
             [segments (decompose-critical-segments g root)]
             [crit-dur (for/sum ([seg (in-list segments)]) (critical-segment-duration seg))]
             [attributions (compute-service-attributions segments total-dur)])
        (critical-path-result tid
                              critical-span-list
                              crit-dur
                              total-dur
                              segments
                              attributions))))

;; Computes the set of span IDs lying on the critical path recursively
(define (compute-critical-span-ids g root-span)
  (define critical-set (mutable-set))
  (set-add! critical-set (span-span-id root-span))
  
  (let walk ([curr-sid (span-span-id root-span)])
    (define children-ids (hash-ref (trace-dag-adj-out g) curr-sid '()))
    (unless (null? children-ids)
      ;; Find child with maximum impact / latest finish time
      (define children-spans
        (map (lambda (cid) (hash-ref (trace-dag-nodes g) cid)) children-ids))
      ;; Select the child that determined the parent's completion
      (define bottleneck-child
        (first (sort children-spans > #:key span-end-time)))
      (set-add! critical-set (span-span-id bottleneck-child))
      (walk (span-span-id bottleneck-child))))
  
  critical-set)

;; Decomposes the critical path into contiguous non-overlapping segments
;; distinguishing between local compute time and downstream wait time.
(define (decompose-critical-segments g root-span)
  (define segments '())
  
  (define (process-span s)
    (define sid (span-span-id s))
    (define svc (span-service-name s))
    (define name (span-name s))
    (define s-start (span-start-time s))
    (define s-end (span-end-time s))
    
    (define child-ids (hash-ref (trace-dag-adj-out g) sid '()))
    (if (null? child-ids)
        ;; Leaf span: entirely compute
        (list (critical-segment sid svc name s-start s-end (- s-end s-start) 'compute))
        (let* ([children (sort (map (lambda (cid) (hash-ref (trace-dag-nodes g) cid)) child-ids)
                               < #:key span-start-time)]
               [child-segments
                ;; Decompose each child
                (apply append (map process-span children))]
               ;; Time before first child started is parent's local compute
               [first-child (first children)]
               [prefix-dur (- (span-start-time first-child) s-start)]
               [prefix-seg
                (if (> prefix-dur 0)
                    (list (critical-segment sid svc (string-append name " [pre-dispatch]")
                                           s-start (span-start-time first-child) prefix-dur 'compute))
                    '())]
               ;; Time after last child finished is parent's post-processing compute
               [last-finish (apply max (map span-end-time children))]
               [suffix-dur (- s-end last-finish)]
               [suffix-seg
                (if (> suffix-dur 0)
                    (list (critical-segment sid svc (string-append name " [post-process]")
                                           last-finish s-end suffix-dur 'compute))
                    '())])
          (append prefix-seg child-segments suffix-seg))))
  
  (define raw-segments (process-span root-span))
  ;; Clean and deduplicate contiguous segments
  (filter (lambda (seg) (> (critical-segment-duration seg) 0)) raw-segments))

;; Computes percentage of critical path attributable to each service
(define (compute-service-attributions segments total-duration)
  (define service-totals (make-hash))
  (for ([seg (in-list segments)])
    (hash-update! service-totals
                  (critical-segment-service-name seg)
                  (lambda (acc) (+ acc (critical-segment-duration seg)))
                  0))
  
  (for/list ([(svc dur) (in-hash service-totals)])
    (define pct (* (/ dur (max 1 total-duration)) 100.0))
    (service-attribution svc dur (round-to-2-decimals pct))))

(define (round-to-2-decimals n)
  (/ (round (* n 100.0)) 100.0))
