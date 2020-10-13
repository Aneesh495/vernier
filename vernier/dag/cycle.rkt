#lang racket/base

;;; vernier/dag/cycle.rkt
;;; Cycle detection using Tarjan Strongly Connected Components (SCC)
;;; and heuristics to sanitize cyclic span topologies.

(require racket/contract
         racket/list
         racket/set
         "graph.rkt"
         "../core/span.rkt")

(provide
 (contract-out
  ;; Tarjan SCC algorithm
  [dag-find-scc (-> trace-dag? (listof (listof string?)))]
  [dag-cyclic? (-> trace-dag? boolean?)]
  [dag-extract-cycles (-> trace-dag? (listof (listof string?)))]
  
  ;; Sanitization / Cycle Breaking
  [struct cycle-break-result ([sanitized-dag trace-dag?]
                              [broken-edges (listof (cons/c string? string?))])]
  [dag-break-cycles (-> trace-dag? cycle-break-result?)]))

(struct cycle-break-result (sanitized-dag broken-edges)
  #:transparent)

;; Tarjan algorithm for finding all Strongly Connected Components
(define (dag-find-scc g)
  (define index 0)
  (define stack '())
  (define in-stack (mutable-set))
  (define indices (make-hash))
  (define lowlinks (make-hash))
  (define sccs '())
  
  (define (strongconnect sid)
    (hash-set! indices sid index)
    (hash-set! lowlinks sid index)
    (set! index (add1 index))
    (set! stack (cons sid stack))
    (set-add! in-stack sid)
    
    (define children (hash-ref (trace-dag-adj-out g) sid '()))
    (for ([cid (in-list children)])
      (cond
        [(not (hash-has-key? indices cid))
         ;; Successor cid has not yet been visited; recurse on it
         (strongconnect cid)
         (hash-set! lowlinks sid (min (hash-ref lowlinks sid)
                                      (hash-ref lowlinks cid)))]
        [(set-member? in-stack cid)
         ;; Successor cid is in stack and hence in the current SCC
         (hash-set! lowlinks sid (min (hash-ref lowlinks sid)
                                      (hash-ref indices cid)))]))
    
    ;; If sid is a root node, pop the stack and generate an SCC
    (when (= (hash-ref lowlinks sid) (hash-ref indices sid))
      (define-values (new-scc rest-stack)
        (let pop-loop ([scc '()] [stk stack])
          (define w (car stk))
          (define rem (cdr stk))
          (set-remove! in-stack w)
          (if (string=? w sid)
              (values (cons w scc) rem)
              (pop-loop (cons w scc) rem))))
      (set! stack rest-stack)
      (set! sccs (cons new-scc sccs))))
  
  (for ([sid (in-hash-keys (trace-dag-nodes g))])
    (unless (hash-has-key? indices sid)
      (strongconnect sid)))
  
  (reverse sccs))

;; Returns true if graph contains any nontrivial strongly connected component
(define (dag-cyclic? g)
  (define sccs (dag-find-scc g))
  (ormap (lambda (scc) (> (length scc) 1)) sccs))

;; Returns all cycles containing two or more vertices
(define (dag-extract-cycles g)
  (define sccs (dag-find-scc g))
  (filter (lambda (scc) (> (length scc) 1)) sccs))

;; Breaks cycles by removing back-edges from spans starting later in time
(define (dag-break-cycles g)
  (if (not (dag-cyclic? g))
      (cycle-break-result g '())
      (let loop ([current-g g]
                 [removed-edges '()])
        (define cycles (dag-extract-cycles current-g))
        (if (null? cycles)
            (cycle-break-result current-g (reverse removed-edges))
            (let* ([scc (car cycles)]
                   [edge-to-remove (select-edge-to-break current-g scc)]
                   [next-g (remove-edge current-g (car edge-to-remove) (cdr edge-to-remove))])
              (loop next-g (cons edge-to-remove removed-edges)))))))

;; Helper to select the most sacrificial edge in a cycle
;; Prefers edge where child start-time is earlier than parent (inverted causality)
(define (select-edge-to-break g scc)
  (define scc-set (list->set scc))
  (define candidates '())
  (for ([u (in-list scc)])
    (define children (hash-ref (trace-dag-adj-out g) u '()))
    (for ([v (in-list children)])
      (when (set-member? scc-set v)
        (set! candidates (cons (cons u v) candidates)))))
  
  (if (null? candidates)
      (cons (car scc) (cadr scc))
      ;; Select candidate with largest start-time inversion
      (first (sort candidates >
                   #:key (lambda (pair)
                           (define u-span (hash-ref (trace-dag-nodes g) (car pair)))
                           (define v-span (hash-ref (trace-dag-nodes g) (cdr pair)))
                           (- (span-start-time u-span) (span-start-time v-span)))))))

;; Internal helper to remove a directed edge (u -> v) from graph
(define (remove-edge g u v)
  (define new-out
    (hash-update (trace-dag-adj-out g) u
                 (lambda (cs) (filter (lambda (c) (not (string=? c v))) cs))))
  (define new-in
    (hash-update (trace-dag-adj-in g) v
                 (lambda (ps) (filter (lambda (p) (not (string=? p u))) ps))))
  (define new-roots
    (for/list ([(sid ps) (in-hash new-in)] #:when (null? ps)) sid))
  (define new-leaves
    (for/list ([(sid cs) (in-hash new-out)] #:when (null? cs)) sid))
  (trace-dag (trace-dag-trace-id g)
             (trace-dag-nodes g)
             new-out
             new-in
             new-roots
             new-leaves))
