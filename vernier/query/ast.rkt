#lang racket/base

;;; vernier/query/ast.rkt
;;; Abstract Syntax Tree (AST) definitions for the Vernier Query Language (VQL).
;;; Represents multi-dimensional boolean filters, comparisons, and aggregations.

(require racket/contract)

(provide
 (contract-out
  ;; AST nodes
  [struct vql-and ([terms list?])]
  [struct vql-or ([terms list?])]
  [struct vql-not ([term any/c])]
  [struct vql-field-cmp ([field symbol?]
                         [op (or/c '= '!= '< '<= '> '>= '~ ':)]
                         [value any/c])]
  [struct vql-tag-cmp ([key string?]
                       [op (or/c '= '!= '~ ':)]
                       [value any/c])]
  [struct vql-wildcard ([pattern string?])]
  
  ;; Constructors
  [make-vql-and (-> list? vql-and?)]
  [make-vql-or (-> list? vql-or?)]
  [make-vql-not (-> any/c vql-not?)]
  [make-vql-field (-> symbol? (or/c '= '!= '< '<= '> '>= '~ ':) any/c vql-field-cmp?)]
  [make-vql-tag (-> string? (or/c '= '!= '~ ':) any/c vql-tag-cmp?)]))

(struct vql-and (terms) #:transparent)
(struct vql-or (terms) #:transparent)
(struct vql-not (term) #:transparent)
(struct vql-field-cmp (field op value) #:transparent)
(struct vql-tag-cmp (key op value) #:transparent)
(struct vql-wildcard (pattern) #:transparent)

(define (make-vql-and terms) (vql-and terms))
(define (make-vql-or terms) (vql-or terms))
(define (make-vql-not term) (vql-not term))
(define (make-vql-field field op value) (vql-field-cmp field op value))
(define (make-vql-tag key op value) (vql-tag-cmp key op value))
