#lang racket/base

;;; vernier/dag/diff.rkt
;;; Structural trace comparison and latency regression differencing.
;;; Detects operational regressions, unexpected branch divergence, and retry loops.

(require racket/contract
         racket/list
         racket/set
         "../core/span.rkt"
         "../core/trace.rkt"
         "graph.rkt")

(provide
 (contract-out
  ;; Diff records
  [struct span-diff ([operation string?]
                     [service string?]
                     [duration-a (or/c exact-integer? #f)]
                     [duration-b (or/c exact-integer? #f)]
                     [duration-delta (or/c exact-integer? #f)]
                     [ratio (or/c real? #f)]
                     [status-diff (or/c (cons/c symbol? symbol?) #f)])]
  
  [struct trace-diff-result ([trace-id-a string?]
                             [trace-id-b string?]
                             [matched-spans (listof span-diff?)]
                             [added-in-b (listof span?)]
                             [removed-in-b (listof span?)]
                             [total-duration-delta exact-integer?]
                             [divergence-summary string?])]
  
  ;; Comparison entrypoint
  [diff-traces (-> trace-record? trace-record? trace-diff-result?)]))

(struct span-diff (operation
                   service
                   duration-a
                   duration-b
                   duration-delta
                   ratio
                   status-diff)
  #:transparent)

(struct trace-diff-result (trace-id-a
                           trace-id-b
                           matched-spans
                           added-in-b
                           removed-in-b
                           total-duration-delta
                           divergence-summary)
  #:transparent)

;; Compares two traces and produces a structured regression diff
(define (diff-traces tr-a tr-b)
  (define id-a (trace-record-trace-id tr-a))
  (define id-b (trace-record-trace-id tr-b))
  
  ;; Create signature per span: (cons service-name name)
  (define (span-signature s) (cons (span-service-name s) (span-name s)))
  
  (define spans-a (trace-record-spans tr-a))
  (define spans-b (trace-record-spans tr-b))
  
  ;; Group spans by signature
  (define map-a (make-hash))
  (for ([s (in-list spans-a)])
    (hash-update! map-a (span-signature s) (lambda (lst) (cons s lst)) '()))
  
  (define map-b (make-hash))
  (for ([s (in-list spans-b)])
    (hash-update! map-b (span-signature s) (lambda (lst) (cons s lst)) '()))
  
  (define matched '())
  (define added '())
  (define removed '())
  
  (define all-sigs (set-union (list->set (hash-keys map-a))
                              (list->set (hash-keys map-b))))
  
  (for ([sig (in-set all-sigs)])
    (define list-a (hash-ref map-a sig '()))
    (define list-b (hash-ref map-b sig '()))
    (cond
      [(and (not (null? list-a)) (not (null? list-b)))
       ;; Match pair by pair
       (let loop ([rem-a list-a] [rem-b list-b])
         (cond
           [(and (not (null? rem-a)) (not (null? rem-b)))
            (define sa (car rem-a))
            (define sb (car rem-b))
            (define da (span-duration sa))
            (define db (span-duration sb))
            (define delta (- db da))
            (define ratio (if (zero? da) 1.0 (/ db (* 1.0 da))))
            (define stat-diff
              (if (eq? (span-status-code sa) (span-status-code sb))
                  #f
                  (cons (span-status-code sa) (span-status-code sb))))
            (set! matched (cons (span-diff (cdr sig) (car sig) da db delta ratio stat-diff) matched))
            (loop (cdr rem-a) (cdr rem-b))]
           [(not (null? rem-a))
            (set! removed (append rem-a removed))]
           [(not (null? rem-b))
            (set! added (append rem-b added))]))]
      [(null? list-a)
       (set! added (append list-b added))]
      [else
       (set! removed (append list-a removed))]))
  
  (define dur-delta (- (trace-record-duration tr-b) (trace-record-duration tr-a)))
  (define summary
    (format "Total duration delta: ~ams (~a% change). Matched: ~a, Added: ~a, Removed: ~a"
            (quotient dur-delta 1000000)
            (round (* 100.0 (/ dur-delta (max 1 (trace-record-duration tr-a)))))
            (length matched)
            (length added)
            (length removed)))
  
  (trace-diff-result id-a id-b (reverse matched) added removed dur-delta summary))
