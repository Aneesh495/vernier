#lang racket/base

;;; tests/unit/test_id.rkt
;;; Unit test suite for TraceId and SpanId generation, validation, and W3C headers.

(require rackunit
         "../../vernier/core/id.rkt")

(test-case "TraceId generation and validity"
  (define tid (make-trace-id))
  (check-true (valid-trace-id? tid))
  (check-equal? (string-length tid) 32)
  (check-false (valid-trace-id? "00000000000000000000000000000000") "All zeros must be invalid")
  (check-false (valid-trace-id? "xyz123") "Short or invalid hex must be rejected")
  (check-false (valid-trace-id? (string-append tid "0")) "Extra character must be rejected"))

(test-case "Time-ordered TraceId generation"
  (define tid-ordered (make-time-ordered-trace-id))
  (check-true (valid-trace-id? tid-ordered))
  (check-equal? (string-length tid-ordered) 32))

(test-case "SpanId generation and validity"
  (define sid (make-span-id))
  (check-true (valid-span-id? sid))
  (check-equal? (string-length sid) 16)
  (check-false (valid-span-id? "0000000000000000") "All zeros must be invalid")
  (check-false (valid-span-id? "12345") "Too short")
  (check-false (valid-span-id? "zzzzzzzzzzzzzzzz") "Non hex"))

(test-case "Bytes and string conversions"
  (define tid (make-trace-id))
  (define tid-bs (trace-id->bytes tid))
  (check-equal? (bytes-length tid-bs) 16)
  (check-equal? (bytes->trace-id tid-bs) tid)
  
  (define sid (make-span-id))
  (define sid-bs (span-id->bytes sid))
  (check-equal? (bytes-length sid-bs) 8)
  (check-equal? (bytes->span-id sid-bs) sid))

(test-case "Trace ID and Span ID hashing"
  (define tid1 (make-trace-id))
  (define tid2 (make-trace-id))
  (check-pred exact-nonnegative-integer? (trace-id-hash tid1))
  (check-pred exact-nonnegative-integer? (span-id-hash (make-span-id))))

(test-case "W3C TraceContext traceparent formatting and parsing"
  (define tid (make-trace-id))
  (define sid (make-span-id))
  (define flags (make-trace-flags #t #f))
  (define header (format-traceparent tid sid flags))
  (check-equal? (string-length header) 55)
  
  (define parsed (parse-traceparent header))
  (check-not-false parsed)
  (check-equal? (car parsed) tid)
  (check-equal? (cadr parsed) sid)
  (check-true (trace-flags-sampled? (caddr parsed)))
  (check-false (trace-flags-debug? (caddr parsed)))
  
  ;; Malformed header parsing tests
  (check-false (parse-traceparent "invalid-header"))
  (check-false (parse-traceparent "01-short-tid-sid-01"))
  (check-false (parse-traceparent (format "00-~a-~a-01" "00000000000000000000000000000000" sid))))
