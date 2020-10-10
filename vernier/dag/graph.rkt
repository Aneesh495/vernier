#lang racket/base

;;; vernier/dag/graph.rkt
;;; Directed Acyclic Graph (DAG) construction, topological sort, and reachability.
;;; Models causal execution graphs across distributed service boundaries.

(require racket/contract
         racket/list
         racket/set
         "../core/span.rkt"
         "../core/trace.rkt")

(provide
 (contract-out
  ;; Graph data structure
  [struct trace-dag ([trace-id string?]
                     [nodes hash?]        ; span-id -> span
                     [adj-out hash?]      ; span-id -> (listof string?) children
                     [adj-in hash?]       ; span-id -> (listof string?) parents
                     [roots (listof string?)]
                     [leaves (listof string?)])]
  
  ;; Constructors
  [trace->dag (-> trace-record? trace-dag?)]
  [spans->dag (-> (listof span?) (or/c trace-dag? #f))]
  
  ;; Traversals and topological ordering
  [dag-topological-sort (-> trace-dag? (or/c (listof string?) #f))]
  [dag-ancestors (-> trace-dag? string? (listof string?))]
  [dag-descendants (-> trace-dag? string? (listof string?))]
  [dag-depth (-> trace-dag? string? exact-nonnegative-integer?)]
  [dag-max-depth (-> trace-dag? exact-nonnegative-integer?)]
  [dag-subgraph (-> trace-dag? (listof string?) trace-dag?)]))

(struct trace-dag (trace-id nodes adj-out adj-in roots leaves)
  #:transparent)

;; Converts a trace-record into a trace-dag
(define (trace->dag tr)
  (define tid (trace-record-trace-id tr))
  (define spans (trace-record-spans tr))
  
  (define node-map
    (for/hash ([s (in-list spans)])
      (values (span-span-id s) s)))
  
  ;; Build adjacency maps
  (define out-map
    (for/fold ([acc (for/hash ([s (in-list spans)]) (values (span-span-id s) '()))])
              ([s (in-list spans)])
      (define pid (span-parent-id s))
      (if (and pid (hash-has-key? node-map pid))
          (hash-update acc pid (lambda (cs) (cons (span-span-id s) cs)))
          acc)))
  
  (define in-map
    (for/fold ([acc (for/hash ([s (in-list spans)]) (values (span-span-id s) '()))])
              ([s (in-list spans)])
      (define pid (span-parent-id s))
      (if (and pid (hash-has-key? node-map pid))
          (hash-update acc (span-span-id s) (lambda (ps) (cons pid ps)))
          acc)))
  
  (define roots
    (for/list ([(sid ps) (in-hash in-map)]
               #:when (null? ps))
      sid))
  
  (define leaves
    (for/list ([(sid cs) (in-hash out-map)]
               #:when (null? cs))
      sid))
  
  (trace-dag tid node-map out-map in-map roots leaves))

;; Convenience wrapper for raw span lists
(define (spans->dag span-list)
  (define tr (make-trace span-list))
  (and tr (trace->dag tr)))

;; Computes topological order using Kahn's algorithm
;; Returns #f if graph has cycles
(define (dag-topological-sort g)
  (define in-degrees
    (for/hash ([(sid ps) (in-hash (trace-dag-adj-in g))])
      (values sid (length ps))))
  
  (define initial-queue
    (for/list ([(sid deg) (in-hash in-degrees)]
               #:when (zero? deg))
      sid))
  
  (let loop ([queue initial-queue]
             [degs in-degrees]
             [result '()])
    (if (null? queue)
        (if (= (length result) (hash-count (trace-dag-nodes g)))
            (reverse result)
            #f) ;; Cycle detected
        (let* ([curr (car queue)]
               [rest-q (cdr queue)]
               [children (hash-ref (trace-dag-adj-out g) curr '())])
          (define-values (new-degs new-zeroes)
            (for/fold ([d degs] [z '()])
                      ([c (in-list children)])
              (define updated-deg (sub1 (hash-ref d c)))
              (values (hash-set d c updated-deg)
                      (if (zero? updated-deg) (cons c z) z))))
          (loop (append rest-q new-zeroes)
                new-degs
                (cons curr result))))))

;; Returns all ancestor span IDs for a given span in top-down order
(define (dag-ancestors g sid)
  (define visited (mutable-set))
  (let loop ([stack (hash-ref (trace-dag-adj-in g) sid '())])
    (unless (null? stack)
      (define curr (car stack))
      (define rest (cdr stack))
      (unless (set-member? visited curr)
        (set-add! visited curr)
        (loop (append rest (hash-ref (trace-dag-adj-in g) curr '()))))))
  (set->list visited))

;; Returns all descendant span IDs for a given span
(define (dag-descendants g sid)
  (define visited (mutable-set))
  (let loop ([stack (hash-ref (trace-dag-adj-out g) sid '())])
    (unless (null? stack)
      (define curr (car stack))
      (define rest (cdr stack))
      (unless (set-member? visited curr)
        (set-add! visited curr)
        (loop (append rest (hash-ref (trace-dag-adj-out g) curr '()))))))
  (set->list visited))

;; Calculates depth of a specific span from the root (0-indexed)
(define (dag-depth g sid)
  (define in-parents (hash-ref (trace-dag-adj-in g) sid '()))
  (if (null? in-parents)
      0
      (add1 (apply max (map (lambda (p) (dag-depth g p)) in-parents)))))

;; Calculates maximum depth (height) of the entire DAG
(define (dag-max-depth g)
  (if (null? (trace-dag-leaves g))
      0
      (apply max (map (lambda (leaf) (dag-depth g leaf)) (trace-dag-leaves g)))))

;; Extracts an induced subgraph retaining only the selected span IDs
(define (dag-subgraph g selected-sids)
  (define keep-set (list->set selected-sids))
  (define sub-nodes
    (for/hash ([(sid s) (in-hash (trace-dag-nodes g))]
               #:when (set-member? keep-set sid))
      (values sid s)))
  (define sub-out
    (for/hash ([(sid cs) (in-hash (trace-dag-adj-out g))]
               #:when (set-member? keep-set sid))
      (values sid (filter (lambda (c) (set-member? keep-set c)) cs))))
  (define sub-in
    (for/hash ([(sid ps) (in-hash (trace-dag-adj-in g))]
               #:when (set-member? keep-set sid))
      (values sid (filter (lambda (p) (set-member? keep-set p)) ps))))
  (define sub-roots
    (for/list ([(sid ps) (in-hash sub-in)]
               #:when (null? ps))
      sid))
  (define sub-leaves
    (for/list ([(sid cs) (in-hash sub-out)]
               #:when (null? cs))
      sid))
  (trace-dag (trace-dag-trace-id g) sub-nodes sub-out sub-in sub-roots sub-leaves))
