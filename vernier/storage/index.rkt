#lang racket/base

;;; vernier/storage/index.rkt
;;; Multi-dimensional inverted index for instantaneous tag and service lookups.
;;; Provides set operations (intersection, union, difference) over trace posting lists.

(require racket/contract
         racket/set
         "../core/span.rkt")

(provide
 (contract-out
  ;; Inverted index structure
  [struct inverted-index ([service-postings hash?]
                          [operation-postings hash?]
                          [status-postings hash?]
                          [tag-postings hash?])]
  
  [make-inverted-index (-> inverted-index?)]
  [index-add-span (-> inverted-index? span? inverted-index?)]
  [index-add-span! (-> (box/c inverted-index?) span? void?)]
  
  ;; Lookup primitives
  [index-query-service (-> inverted-index? string? (set/c string?))]
  [index-query-operation (-> inverted-index? string? (set/c string?))]
  [index-query-status (-> inverted-index? symbol? (set/c string?))]
  [index-query-tag (-> inverted-index? any/c any/c (set/c string?))]
  
  ;; Set combinators
  [index-intersect (-> (set/c string?) (set/c string?) (set/c string?))]
  [index-union (-> (set/c string?) (set/c string?) (set/c string?))]
  [index-difference (-> (set/c string?) (set/c string?) (set/c string?))]))

(struct inverted-index (service-postings
                        operation-postings
                        status-postings
                        tag-postings)
  #:transparent)

(define (make-inverted-index)
  (inverted-index (hash) (hash) (hash) (hash)))

;; Helper to add a trace-id into posting map
(define (postings-add postings-map key tid)
  (hash-update postings-map key
               (lambda (s) (set-add s tid))
               (set)))

;; Indexes a single span
(define (index-add-span idx s)
  (define tid (span-trace-id s))
  (define new-svc (postings-add (inverted-index-service-postings idx) (span-service-name s) tid))
  (define new-op (postings-add (inverted-index-operation-postings idx) (span-name s) tid))
  (define new-stat (postings-add (inverted-index-status-postings idx) (span-status-code s) tid))
  
  ;; Index attributes
  (define new-tags
    (for/fold ([acc (inverted-index-tag-postings idx)])
              ([(k v) (in-hash (span-attributes s))])
      (define tag-key (cons k v))
      (postings-add acc tag-key tid)))
  
  (inverted-index new-svc new-op new-stat new-tags))

(define (index-add-span! box-idx s)
  (set-box! box-idx (index-add-span (unbox box-idx) s)))

;; Query helpers
(define (index-query-service idx svc)
  (hash-ref (inverted-index-service-postings idx) svc (set)))

(define (index-query-operation idx op)
  (hash-ref (inverted-index-operation-postings idx) op (set)))

(define (index-query-status idx stat)
  (hash-ref (inverted-index-status-postings idx) stat (set)))

(define (index-query-tag idx key val)
  (hash-ref (inverted-index-tag-postings idx) (cons key val) (set)))

;; Set combinators
(define (index-intersect s1 s2)
  (set-intersect s1 s2))

(define (index-union s1 s2)
  (set-union s1 s2))

(define (index-difference s1 s2)
  (set-subtract s1 s2))
