#lang racket/base

;;; vernier/sampler/tail_buffer.rkt
;;; In-memory trace assembly buffer with inactivity and expiration timeouts.
;;; Collects asynchronous out-of-order spans prior to tail-sampling decisions.

(require racket/contract
         racket/list
         "../core/time.rkt"
         "../core/span.rkt"
         "../core/trace.rkt")

(provide
 (contract-out
  ;; Buffer structure
  [struct trace-buffer-entry ([trace-id string?]
                              [spans (listof span?)]
                              [first-seen-nanos exact-integer?]
                              [last-seen-nanos exact-integer?])]
  
  [struct tail-buffer ([capacity exact-positive-integer?]
                       [inactivity-nanos exact-integer?]
                       [max-age-nanos exact-integer?]
                       [entries hash?])]
  
  [make-tail-buffer (->* () (exact-positive-integer? exact-integer? exact-integer?) tail-buffer?)]
  [tail-buffer-insert-span (-> tail-buffer? span? exact-integer? tail-buffer?)]
  [tail-buffer-insert-span! (-> (box/c tail-buffer?) span? exact-integer? void?)]
  [tail-buffer-reap-completed (-> tail-buffer? exact-integer? (values tail-buffer? (listof trace-record?)))]
  [tail-buffer-count (-> tail-buffer? exact-nonnegative-integer?)]))

(struct trace-buffer-entry (trace-id spans first-seen-nanos last-seen-nanos)
  #:transparent)

(struct tail-buffer (capacity inactivity-nanos max-age-nanos entries)
  #:transparent)

;; Constructs a new tail buffer
;; Default: capacity 10000 traces, 500ms inactivity completion, 10s max age
(define (make-tail-buffer [capacity 10000]
                          [inactivity-nanos 500000000]
                          [max-age-nanos 10000000000])
  (tail-buffer capacity inactivity-nanos max-age-nanos (hash)))

;; Inserts span into buffer under its trace-id
(define (tail-buffer-insert-span tb s now-nanos)
  (define tid (span-trace-id s))
  (define current-entries (tail-buffer-entries tb))
  
  (define updated-entry
    (let ([existing (hash-ref current-entries tid #f)])
      (if existing
          (struct-copy trace-buffer-entry existing
                       [spans (cons s (trace-buffer-entry-spans existing))]
                       [last-seen-nanos now-nanos])
          (trace-buffer-entry tid (list s) now-nanos now-nanos))))
  
  ;; Capacity eviction if full and new trace arriving
  (define trimmed-entries
    (if (and (not (hash-has-key? current-entries tid))
             (>= (hash-count current-entries) (tail-buffer-capacity tb)))
        ;; Evict oldest entry
        (let* ([oldest-tid (car (first (sort (hash->list current-entries) <
                                             #:key (lambda (p) (trace-buffer-entry-first-seen-nanos (cdr p))))))])
          (hash-remove current-entries oldest-tid))
        current-entries))
  
  (struct-copy tail-buffer tb
               [entries (hash-set trimmed-entries tid updated-entry)]))

(define (tail-buffer-insert-span! box-tb s now-nanos)
  (set-box! box-tb (tail-buffer-insert-span (unbox box-tb) s now-nanos)))

;; Reaps traces ready for sampling: inactive for inactivity-nanos or exceeding max-age
(define (tail-buffer-reap-completed tb now-nanos)
  (define inact-thresh (tail-buffer-inactivity-nanos tb))
  (define max-age-thresh (tail-buffer-max-age-nanos tb))
  
  (define-values (reaped-traces remaining-entries)
    (for/fold ([reaped '()] [rem (hash)])
              ([(tid entry) (in-hash (tail-buffer-entries tb))])
      (define age (- now-nanos (trace-buffer-entry-first-seen-nanos entry)))
      (define inactive-for (- now-nanos (trace-buffer-entry-last-seen-nanos entry)))
      
      (if (or (> age max-age-thresh)
              (and (> inactive-for inact-thresh)
                   (ormap span-root? (trace-buffer-entry-spans entry))))
          (let ([tr (make-trace (trace-buffer-entry-spans entry))])
            (values (if tr (cons tr reaped) reaped) rem))
          (values reaped (hash-set rem tid entry)))))
  
  (values (struct-copy tail-buffer tb [entries remaining-entries])
          (reverse reaped-traces)))

(define (tail-buffer-count tb)
  (hash-count (tail-buffer-entries tb)))
