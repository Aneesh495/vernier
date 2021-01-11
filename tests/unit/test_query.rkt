#lang racket/base

;;; tests/unit/test_query.rkt
;;; Unit test suite for VQL lexer, recursive-descent parser, and query executor.

(require rackunit
         racket/list
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/query/ast.rkt"
         "../../vernier/query/lexer.rkt"
         "../../vernier/query/parser.rkt"
         "../../vernier/query/executor.rkt")

(test-case "Duration literal lexer"
  (check-equal? (parse-duration-literal "500ns") 500)
  (check-equal? (parse-duration-literal "250us") 250000)
  (check-equal? (parse-duration-literal "100ms") 100000000)
  (check-equal? (parse-duration-literal "2s") 2000000000)
  (check-false (parse-duration-literal "invalid")))

(test-case "VQL Tokenizer"
  (define tokens (tokenize-vql "service:auth AND duration > 100ms AND NOT status:error"))
  (check-true (> (length tokens) 5))
  (check-equal? (token-type (first tokens)) 'ident)
  (check-equal? (token-value (first tokens)) "service")
  (check-equal? (token-type (second tokens)) 'colon))

(test-case "VQL Parser produces correct AST"
  (define ast1 (parse-vql "service:checkout AND status:error"))
  (check-pred vql-and? ast1)
  (check-equal? (length (vql-and-terms ast1)) 2)
  
  (define ast2 (parse-vql "duration >= 50ms OR (service:auth AND NOT status:error)"))
  (check-pred vql-or? ast2)
  (check-equal? (length (vql-or-terms ast2)) 2)
  
  (define ast-tag (parse-vql "tag.user = \"alice\""))
  (check-pred vql-tag-cmp? ast-tag)
  (check-equal? (vql-tag-cmp-key ast-tag) "user")
  (check-equal? (vql-tag-cmp-value ast-tag) "alice"))

(test-case "VQL Query Execution against trace collection"
  (define tid1 (make-trace-id))
  (define tid2 (make-trace-id))
  (define tid3 (make-trace-id))
  
  (define s1 (make-span #:trace-id tid1 #:span-id (make-span-id) #:name "GET /orders"
                        #:service-name "gateway" #:start-time 1000 #:end-time (+ 1000 50000000) ;; 50ms
                        #:status-code 'ok #:attributes (hasheq 'env "prod")))
  
  (define s2 (make-span #:trace-id tid2 #:span-id (make-span-id) #:name "POST /charge"
                        #:service-name "payments" #:start-time 2000 #:end-time (+ 2000 300000000) ;; 300ms
                        #:status-code 'error #:attributes (hasheq 'env "prod")))
  
  (define s3 (make-span #:trace-id tid3 #:span-id (make-span-id) #:name "GET /health"
                        #:service-name "gateway" #:start-time 3000 #:end-time (+ 3000 5000000) ;; 5ms
                        #:status-code 'ok #:attributes (hasheq 'env "staging")))
  
  (define traces (list (make-trace (list s1))
                       (make-trace (list s2))
                       (make-trace (list s3))))
  
  ;; Query 1: service:gateway
  (define res1 (execute-vql "service:gateway" traces))
  (check-equal? (length (query-result-traces res1)) 2)
  (check-equal? (query-metrics-total-matches (query-result-metrics res1)) 2)
  
  ;; Query 2: status:error
  (define res2 (execute-vql "status:error" traces))
  (check-equal? (length (query-result-traces res2)) 1)
  (check-equal? (trace-record-trace-id (first (query-result-traces res2))) tid2)
  
  ;; Query 3: duration > 100ms
  (define res3 (execute-vql "duration > 100ms" traces))
  (check-equal? (length (query-result-traces res3)) 1)
  (check-equal? (trace-record-trace-id (first (query-result-traces res3))) tid2)
  
  ;; Query 4: tag.env = "prod" AND NOT status:error
  (define res4 (execute-vql "tag.env = \"prod\" AND NOT status:error" traces))
  (check-equal? (length (query-result-traces res4)) 1)
  (check-equal? (trace-record-trace-id (first (query-result-traces res4))) tid1))
