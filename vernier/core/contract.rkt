#lang racket/base

;;; vernier/core/contract.rkt
;;; Structural contracts, causality verification, and integrity checkers.
;;; Validates that trace graphs satisfy physical and logical invariants.

(require racket/contract
         racket/set
         "id.rkt"
         "time.rkt"
         "span.rkt"
         "trace.rkt")

(provide
 (contract-out
  ;; Invariant violation struct
  [struct integrity-violation ([code symbol?]
                               [severity (or/c 'warning 'error)]
                               [span-id (or/c string? #f)]
                               [message string?])]
  [make-integrity-violation (-> symbol? (or/c 'warning 'error) (or/c string? #f) string? integrity-violation?)]
  
  ;; Validation routines
  [verify-trace-integrity (-> trace-record? (listof integrity-violation?))]
  [trace-strictly-valid? (-> trace-record? boolean?)]
  [check-causality (-> span? span? (or/c integrity-violation? #f))]))

(struct integrity-violation (code severity span-id message)
  #:transparent)

(define (make-integrity-violation code severity span-id message)
  (integrity-violation code severity span-id message))

;; Checks causality between parent span and child span
(define (check-causality parent child)
  (cond
    ;; Child cannot start strictly before parent begins
    [(< (span-start-time child) (span-start-time parent))
     (make-integrity-violation
      'time-inversion-start
      'warning
      (span-span-id child)
      (format "Child span ~a started (~a) before parent ~a (~a)"
              (span-span-id child)
              (span-start-time child)
              (span-span-id parent)
              (span-start-time parent)))]
    ;; Child cannot end strictly after parent finishes (for synchronous spans)
    [(and (not (eq? (span-kind child) 'consumer))
          (> (span-end-time child) (span-end-time parent)))
     (make-integrity-violation
      'time-inversion-end
      'warning
      (span-span-id child)
      (format "Child span ~a finished (~a) after parent ~a (~a)"
              (span-span-id child)
              (span-end-time child)
              (span-span-id parent)
              (span-end-time parent)))]
    [else #f]))

;; Verifies full structural integrity of a trace record
(define (verify-trace-integrity tr)
  (define violations '())
  
  ;; Rule 1: Must have at least one root span
  (define roots (trace-find-roots tr))
  (when (null? roots)
    (set! violations (cons (make-integrity-violation
                            'no-root-found 'error #f "Trace has no identifiable root span")
                           violations)))
  
  ;; Rule 2: Multi-root warning if trace has disjoint forest components
  (when (> (length roots) 1)
    (set! violations (cons (make-integrity-violation
                            'multiple-roots 'warning #f
                            (format "Trace contains ~a disconnected root components" (length roots)))
                           violations)))
  
  ;; Rule 3: Check every child against its parent
  (for ([s (in-list (trace-record-spans tr))])
    (define pid (span-parent-id s))
    (when pid
      (define parent (trace-find-span tr pid))
      (if (not parent)
          (set! violations (cons (make-integrity-violation
                                  'missing-parent 'warning (span-span-id s)
                                  (format "Parent span ~a is missing from trace" pid))
                                 violations))
          (let ([causality-violation (check-causality parent s)])
            (when causality-violation
              (set! violations (cons causality-violation violations)))))))
  
  ;; Rule 4: Cycle detection in parent hierarchy
  (define visited (mutable-set))
  (define rec-stack (mutable-set))
  (define (dfs-has-cycle? sid)
    (set-add! visited sid)
    (set-add! rec-stack sid)
    (define children (map span-span-id (trace-find-children tr sid)))
    (define cycle-found?
      (for/or ([cid (in-list children)])
        (cond
          [(not (set-member? visited cid))
           (dfs-has-cycle? cid)]
          [(set-member? rec-stack cid)
           #t]
          [else #f])))
    (set-remove! rec-stack sid)
    cycle-found?)
  
  (for ([r (in-list roots)])
    (when (dfs-has-cycle? (span-span-id r))
      (set! violations (cons (make-integrity-violation
                              'cyclic-dependency 'error (span-span-id r)
                              "Trace span hierarchy contains a cycle")
                             violations))))
  
  (reverse violations))

;; Returns true if trace contains zero error-level violations
(define (trace-strictly-valid? tr)
  (define vios (verify-trace-integrity tr))
  (not (ormap (lambda (v) (eq? (integrity-violation-severity v) 'error)) vios)))
