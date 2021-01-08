#lang racket/base

;;; vernier/query/executor.rkt
;;; VQL query evaluation engine and result aggregator.
;;; Evaluates AST filter trees against in-memory trace graphs and columnar segments.

(require racket/contract
         racket/list
         racket/string
         racket/match
         "../core/span.rkt"
         "../core/trace.rkt"
         "../sketch/quantile.rkt"
         "ast.rkt"
         "parser.rkt")

(provide
 (contract-out
  ;; Query result summary
  [struct query-metrics ([total-matches exact-nonnegative-integer?]
                         [error-count exact-nonnegative-integer?]
                         [p50-nanos (or/c real? #f)]
                         [p90-nanos (or/c real? #f)]
                         [p99-nanos (or/c real? #f)]
                         [service-counts hash?])]
  
  [struct query-result ([traces (listof trace-record?)]
                        [metrics query-metrics?])]
  
  [trace-matches-ast? (-> trace-record? any/c boolean?)]
  [execute-vql (->* (string? (listof trace-record?)) (#:limit exact-positive-integer?) query-result?)]))

(struct query-metrics (total-matches error-count p50-nanos p90-nanos p99-nanos service-counts)
  #:transparent)

(struct query-result (traces metrics)
  #:transparent)

;; Evaluates whether an individual trace-record satisfies an AST query node
(define (trace-matches-ast? tr ast)
  (match ast
    [(vql-and terms)
     (andmap (lambda (term) (trace-matches-ast? tr term)) terms)]
    
    [(vql-or terms)
     (ormap (lambda (term) (trace-matches-ast? tr term)) terms)]
    
    [(vql-not term)
     (not (trace-matches-ast? tr term))]
    
    [(vql-field-cmp field op value)
     (eval-field-cmp tr field op value)]
    
    [(vql-tag-cmp key op value)
     (eval-tag-cmp tr key op value)]
    
    [(vql-wildcard pattern)
     (eval-wildcard tr pattern)]
    
    [_ #t]))

;; Evaluates built-in field comparison
(define (eval-field-cmp tr field op value)
  (case field
    [(service)
     (define target-svc (format "~a" value))
     (match op
       [(or ': '=) (and (member target-svc (trace-record-services tr)) #t)]
       ['!= (not (member target-svc (trace-record-services tr)))]
       ['~ (ormap (lambda (s) (string-contains? (string-downcase s) (string-downcase target-svc)))
                  (trace-record-services tr))]
       [_ #f])]
    
    [(operation name)
     (define target-op (format "~a" value))
     (match op
       [(or ': '=)
        (ormap (lambda (s) (string-ci=? (span-name s) target-op)) (trace-record-spans tr))]
       ['!=
        (not (ormap (lambda (s) (string-ci=? (span-name s) target-op)) (trace-record-spans tr)))]
       ['~
        (ormap (lambda (s) (string-contains? (string-downcase (span-name s)) (string-downcase target-op)))
               (trace-record-spans tr))]
       [_ #f])]
    
    [(status)
     (define stat-sym
       (cond
         [(symbol? value) value]
         [(string? value) (string->symbol (string-downcase value))]
         [else 'unset]))
     (match op
       [(or ': '=)
        (if (eq? stat-sym 'error)
            (trace-has-errors? tr)
            (not (trace-has-errors? tr)))]
       ['!=
        (if (eq? stat-sym 'error)
            (not (trace-has-errors? tr))
            (trace-has-errors? tr))]
       [_ #f])]
    
    [(duration)
     (define num-val (if (number? value) value 0))
     (define dur (trace-record-duration tr))
     (match op
       ['= (= dur num-val)]
       ['!= (not (= dur num-val))]
       ['> (> dur num-val)]
       ['>= (>= dur num-val)]
       ['< (< dur num-val)]
       ['<= (<= dur num-val)]
       [_ #f])]
    
    [else #f]))

;; Evaluates tag key-value matching
(define (eval-tag-cmp tr key op value)
  (define target-val (format "~a" value))
  (for/or ([s (in-list (trace-record-spans tr))])
    (define v (span-get-attribute s key))
    (and v
         (let ([v-str (format "~a" v)])
           (match op
             [(or ': '=) (string-ci=? v-str target-val)]
             ['!= (not (string-ci=? v-str target-val))]
             ['~ (string-contains? (string-downcase v-str) (string-downcase target-val))]
             [_ #f])))))

;; Wildcard text search across trace summary and span names
(define (eval-wildcard tr pattern)
  (define clean (string-downcase pattern))
  (or (string-contains? (string-downcase (trace-record-trace-id tr)) clean)
      (ormap (lambda (s)
               (or (string-contains? (string-downcase (span-name s)) clean)
                   (string-contains? (string-downcase (span-service-name s)) clean)))
             (trace-record-spans tr))))

;; Executes query string against collection of trace records
(define (execute-vql query-str traces #:limit [limit 100])
  (define ast (parse-vql query-str))
  (define matched (filter (lambda (tr) (trace-matches-ast? tr ast)) traces))
  
  ;; Compute metrics
  (define total (length matched))
  (define errs (count trace-has-errors? matched))
  (define sketch (make-dd-sketch 0.02))
  (define svc-counts (make-hash))
  
  (for ([tr (in-list matched)])
    (dd-sketch-insert! (box sketch) (trace-record-duration tr))
    (for ([svc (in-list (trace-record-services tr))])
      (hash-update! svc-counts svc add1 0)))
  
  (define p50 (dd-sketch-quantile sketch 0.50))
  (define p90 (dd-sketch-quantile sketch 0.90))
  (define p99 (dd-sketch-quantile sketch 0.99))
  
  (define limited-traces (take (sort matched > #:key trace-record-start-time)
                               (min limit total)))
  
  (query-result limited-traces
                (query-metrics total errs p50 p90 p99 svc-counts)))
