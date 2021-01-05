#lang racket/base

;;; vernier/query/parser.rkt
;;; Recursive-descent parser for the Vernier Query Language (VQL).
;;; Compiles token streams into structured AST expression trees.

(require racket/contract
         racket/match
         racket/string
         "ast.rkt"
         "lexer.rkt")

(provide
 (contract-out
  [parse-vql (-> string? any/c)]))

;; Parses a VQL query string into an AST
(define (parse-vql input)
  (define tokens (tokenize-vql input))
  (if (null? tokens)
      (make-vql-and '()) ;; Empty query matches everything
      (let-values ([(ast remaining) (parse-or-expr tokens)])
        (unless (null? remaining)
          (error 'parse-vql "Unexpected tokens following query: ~a" remaining))
        ast)))

;; OR expression: expr (OR expr)*
(define (parse-or-expr tokens)
  (let-values ([(first-term rem) (parse-and-expr tokens)])
    (let loop ([current-term first-term] [ts rem] [or-terms (list first-term)])
      (if (and (not (null? ts)) (eq? (token-type (car ts)) 'or))
          (let-values ([(next-term rest-ts) (parse-and-expr (cdr ts))])
            (loop next-term rest-ts (cons next-term or-terms)))
          (values (if (> (length or-terms) 1)
                      (make-vql-or (reverse or-terms))
                      first-term)
                  ts)))))

;; AND expression: expr (AND? expr)*
(define (parse-and-expr tokens)
  (let-values ([(first-term rem) (parse-unary-expr tokens)])
    (let loop ([current-term first-term] [ts rem] [and-terms (list first-term)])
      (cond
        [(null? ts)
         (values (if (> (length and-terms) 1) (make-vql-and (reverse and-terms)) first-term) ts)]
        [(eq? (token-type (car ts)) 'and)
         (let-values ([(next-term rest-ts) (parse-unary-expr (cdr ts))])
           (loop next-term rest-ts (cons next-term and-terms)))]
        ;; Implicit AND if not OR and not closing paren
        [(not (member (token-type (car ts)) '(or rparen)))
         (let-values ([(next-term rest-ts) (parse-unary-expr ts)])
           (loop next-term rest-ts (cons next-term and-terms)))]
        [else
         (values (if (> (length and-terms) 1) (make-vql-and (reverse and-terms)) first-term) ts)]))))

;; Unary NOT: (NOT expr) | primary
(define (parse-unary-expr tokens)
  (cond
    [(null? tokens)
     (error 'parse-vql "Unexpected end of query expression")]
    [(eq? (token-type (car tokens)) 'not)
     (let-values ([(sub-term rem) (parse-primary-expr (cdr tokens))])
       (values (make-vql-not sub-term) rem))]
    [else
     (parse-primary-expr tokens)]))

;; Primary expression: (expr) | field_cmp | tag_cmp | bare_word
(define (parse-primary-expr tokens)
  (cond
    [(null? tokens)
     (error 'parse-vql "Expected primary expression, got EOF")]
    ;; Parenthesized expression
    [(eq? (token-type (car tokens)) 'lparen)
     (let-values ([(sub-ast rem) (parse-or-expr (cdr tokens))])
       (if (and (not (null? rem)) (eq? (token-type (car rem)) 'rparen))
           (values sub-ast (cdr rem))
           (error 'parse-vql "Missing closing parenthesis in query")))]
    ;; Field comparison: ident (: | op) value
    [(and (eq? (token-type (car tokens)) 'ident)
          (not (null? (cdr tokens)))
          (or (eq? (token-type (cadr tokens)) 'colon)
              (eq? (token-type (cadr tokens)) 'op)))
     (define field-name (string-downcase (token-value (car tokens))))
     (define op-tok (cadr tokens))
     (define op (if (eq? (token-type op-tok) 'colon) ': (token-value op-tok)))
     (define val-tokens (cddr tokens))
     (when (null? val-tokens)
       (error 'parse-vql "Expected value after operator for field ~a" field-name))
     (define val-tok (car val-tokens))
     (define val (token-value val-tok))
     
     ;; Check if field is tag:key
     (if (string-prefix? field-name "tag.")
         (let ([tag-key (substring field-name 4)])
           (values (make-vql-tag tag-key op val) (cdr val-tokens)))
         (values (make-vql-field (string->symbol field-name) op val) (cdr val-tokens)))]
    ;; Bare literal / wildcard
    [else
     (define tok (car tokens))
     (values (vql-wildcard (format "~a" (token-value tok))) (cdr tokens))]))
