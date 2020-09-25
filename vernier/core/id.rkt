#lang racket/base

;;; vernier/core/id.rkt
;;; 128-bit TraceId and 64-bit SpanId generators, validation, and representations.
;;; Adheres to W3C TraceContext specification standards.

(require racket/contract
         racket/format
         racket/string
         racket/random)

(provide
 (contract-out
  ;; TraceId constructors and validators
  [make-trace-id (-> string?)]
  [make-time-ordered-trace-id (-> string?)]
  [valid-trace-id? (-> any/c boolean?)]
  [trace-id->bytes (-> string? bytes?)]
  [bytes->trace-id (-> bytes? string?)]
  [trace-id-hash (-> string? exact-nonnegative-integer?)]
  
  ;; SpanId constructors and validators
  [make-span-id (-> string?)]
  [valid-span-id? (-> any/c boolean?)]
  [span-id->bytes (-> string? bytes?)]
  [bytes->span-id (-> bytes? string?)]
  [span-id-hash (-> string? exact-nonnegative-integer?)]
  
  ;; W3C TraceContext traceparent parsing and formatting
  [struct trace-flags ([sampled? boolean?] [debug? boolean?])]
  [make-trace-flags (-> boolean? boolean? trace-flags?)]
  [default-trace-flags trace-flags?]
  [trace-flags->byte (-> trace-flags? exact-nonnegative-integer?)]
  [byte->trace-flags (-> exact-nonnegative-integer? trace-flags?)]
  [format-traceparent (-> string? string? trace-flags? string?)]
  [parse-traceparent (-> string? (or/c (list/c string? string? trace-flags?) #f))]))

;; Struct representing the 8-bit trace flags field
(struct trace-flags (sampled? debug?)
  #:transparent)

(define (make-trace-flags sampled? debug?)
  (trace-flags sampled? debug?))

(define default-trace-flags
  (trace-flags #t #f))

;; Converts trace-flags struct into single 8-bit integer
(define (trace-flags->byte flags)
  (bitwise-ior (if (trace-flags-sampled? flags) #x01 0)
               (if (trace-flags-debug? flags) #x02 0)))

;; Reconstructs trace-flags struct from an 8-bit integer
(define (byte->trace-flags b)
  (trace-flags (not (zero? (bitwise-and b #x01)))
               (not (zero? (bitwise-and b #x02)))))

;; Validates a hexadecimal string of exact length with no non-hex characters
;; and rejecting all-zero sequences per W3C specification.
(define (valid-hex-id? str expected-len)
  (and (string? str)
       (= (string-length str) expected-len)
       (let loop ([i 0] [all-zero? #t])
         (if (= i expected-len)
             (not all-zero?) ;; All-zero IDs are invalid per spec
             (let ([c (string-ref str i)])
               (and (or (and (char>=? c #\0) (char<=? c #\9))
                        (and (char>=? c #\a) (char<=? c #\f)))
                    (loop (add1 i) (and all-zero? (char=? c #\0)))))))))

;; Validates a 128-bit (32 hex characters) trace ID
(define (valid-trace-id? x)
  (valid-hex-id? x 32))

;; Validates a 64-bit (16 hex characters) span ID
(define (valid-span-id? x)
  (valid-hex-id? x 16))

;; Generates a secure random 128-bit TraceId represented as 32 lowercase hex characters
(define (make-trace-id)
  (define raw-bytes (crypto-random-bytes 16))
  (bytes->hex-string raw-bytes))

;; Generates a time-ordered 128-bit TraceId:
;; Top 64 bits represent millisecond timestamp, lower 64 bits represent entropy.
;; Maximizes B-Tree and LSM-tree index locality in columnar stores.
(define (make-time-ordered-trace-id)
  (define ms (current-seconds))
  (define time-bytes (integer->integer-bytes ms 8 #f #t))
  (define rand-bytes (crypto-random-bytes 8))
  (bytes->hex-string (bytes-append time-bytes rand-bytes)))

;; Generates a secure random 64-bit SpanId represented as 16 lowercase hex characters
(define (make-span-id)
  (define raw-bytes (crypto-random-bytes 8))
  (bytes->hex-string raw-bytes))

;; Helper to convert arbitrary byte string to lowercase hex string
(define (bytes->hex-string bs)
  (define len (bytes-length bs))
  (define out (make-string (* len 2)))
  (define hex-chars "0123456789abcdef")
  (for ([i (in-range len)])
    (define b (bytes-ref bs i))
    (string-set! out (* i 2) (string-ref hex-chars (arithmetic-shift b -4)))
    (string-set! out (+ (* i 2) 1) (string-ref hex-chars (bitwise-and b #x0f))))
  out)

;; Helper to convert hex string to byte array
(define (hex-string->bytes str)
  (define len (string-length str))
  (unless (even? len)
    (error 'hex-string->bytes "Hex string length must be even: ~a" str))
  (define out (make-bytes (quotient len 2)))
  (for ([i (in-range (quotient len 2))])
    (define sub (substring str (* i 2) (+ (* i 2) 2)))
    (bytes-set! out i (or (string->number sub 16) 0)))
  out)

(define (trace-id->bytes tid)
  (hex-string->bytes tid))

(define (bytes->trace-id bs)
  (bytes->hex-string bs))

(define (span-id->bytes sid)
  (hex-string->bytes sid))

(define (bytes->span-id bs)
  (bytes->hex-string bs))

;; Computes 64-bit hash of trace ID for ring buffer sharding
(define (trace-id-hash tid)
  (define bs (trace-id->bytes tid))
  (integer-bytes->integer (subbytes bs 0 8) #f #t))

;; Computes 64-bit hash of span ID
(define (span-id-hash sid)
  (define bs (span-id->bytes sid))
  (integer-bytes->integer bs #f #t))

;; Formats W3C TraceContext traceparent header: 00-{traceid}-{spanid}-{flags}
(define (format-traceparent trace-id span-id flags)
  (format "00-~a-~a-~a"
          trace-id
          span-id
          (~r (trace-flags->byte flags) #:base 16 #:min-width 2 #:pad-string "0")))

;; Parses W3C TraceContext traceparent string
(define (parse-traceparent str)
  (define parts (string-split (string-trim str) "-"))
  (if (and (= (length parts) 4)
           (string=? (car parts) "00")
           (valid-trace-id? (cadr parts))
           (valid-span-id? (caddr parts))
           (= (string-length (cadddr parts)) 2))
      (let ([flags-byte (string->number (cadddr parts) 16)])
        (if flags-byte
            (list (cadr parts) (caddr parts) (byte->trace-flags flags-byte))
            #f))
      #f))
