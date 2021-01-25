#lang racket/base

;;; vernier/protocol/w3c.rkt
;;; W3C TraceContext traceparent and tracestate header propagation.
;;; Implements distributed context propagation across HTTP boundaries.

(require racket/contract
         racket/string
         "../core/id.rkt")

(provide
 (contract-out
  ;; Context holder
  [struct trace-context ([trace-id string?]
                         [span-id string?]
                         [flags trace-flags?]
                         [tracestate hash?])]
  
  [make-trace-context (->* (string? string?) (trace-flags? hash?) trace-context?)]
  [inject-w3c-headers (-> trace-context? (listof (cons/c string? string?)))]
  [extract-w3c-headers (-> (listof (cons/c string? string?)) (or/c trace-context? #f))]))

(struct trace-context (trace-id span-id flags tracestate)
  #:transparent)

(define (make-trace-context trace-id span-id [flags default-trace-flags] [tracestate (hash)])
  (trace-context trace-id span-id flags tracestate))

;; Serializes tracestate hash into key1=val1,key2=val2 string
(define (format-tracestate ts)
  (string-join
   (for/list ([(k v) (in-hash ts)])
     (format "~a=~a" k v))
   ","))

;; Injects trace context into HTTP header key-value pairs
(define (inject-w3c-headers ctx)
  (define tp (format-traceparent (trace-context-trace-id ctx)
                                 (trace-context-span-id ctx)
                                 (trace-context-flags ctx)))
  (define base-headers (list (cons "traceparent" tp)))
  (if (zero? (hash-count (trace-context-tracestate ctx)))
      base-headers
      (cons (cons "tracestate" (format-tracestate (trace-context-tracestate ctx)))
            base-headers)))

;; Parses tracestate header into hash
(define (parse-tracestate str)
  (define pairs (string-split (string-trim str) ","))
  (for/fold ([acc (hash)]) ([p (in-list pairs)])
    (define kv (string-split (string-trim p) "="))
    (if (= (length kv) 2)
        (hash-set acc (string-trim (car kv)) (string-trim (cadr kv)))
        acc)))

;; Extracts trace context from incoming HTTP headers
(define (extract-w3c-headers headers)
  (define lower-headers
    (for/hash ([h (in-list headers)])
      (values (string-downcase (car h)) (cdr h))))
  
  (define tp-raw (hash-ref lower-headers "traceparent" #f))
  (if (not tp-raw)
      #f
      (let ([parsed-tp (parse-traceparent tp-raw)])
        (if (not parsed-tp)
            #f
            (let* ([tid (car parsed-tp)]
                   [sid (cadr parsed-tp)]
                   [flags (caddr parsed-tp)]
                   [ts-raw (hash-ref lower-headers "tracestate" #f)]
                   [ts (if ts-raw (parse-tracestate ts-raw) (hash))])
              (trace-context tid sid flags ts))))))
