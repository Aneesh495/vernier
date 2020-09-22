#lang racket/base

;;; tests/unit/test_time.rkt
;;; Unit test suite for high-resolution timing, intervals, and conversions.

(require rackunit
         "../../vernier/core/time.rkt")

(test-case "High-resolution time generation"
  (define t1 (current-time-nanos))
  (define m1 (current-monotonic-nanos))
  (check-pred exact-integer? t1)
  (check-pred exact-integer? m1)
  (check-true (> t1 1000000000000000000) "Timestamp should be well past epoch in nanos")
  (check-true (> m1 0) "Monotonic nanos must be strictly positive"))

(test-case "Nanosecond unit conversions"
  (check-equal? (nanos->micros 2500000) 2500)
  (check-equal? (nanos->millis 5000000000) 5000)
  (check-equal? (millis->nanos 42) 42000000)
  (check-equal? (micros->nanos 100) 100000)
  (check-equal? (seconds->nanos 1.5) 1500000000)
  (check-equal? (nanos->seconds 2000000000) 2.0))

(test-case "Time interval arithmetic and containment"
  (define iv1 (make-time-interval 1000 2000))
  (define iv2 (make-time-interval 1500 2500))
  (define iv3 (make-time-interval 3000 4000))
  
  (check-equal? (interval-duration iv1) 1000)
  (check-true (interval-contains? iv1 1000))
  (check-true (interval-contains? iv1 1999))
  (check-false (interval-contains? iv1 2000))
  (check-false (interval-contains? iv1 500))
  
  ;; Overlap and intersection
  (check-true (intervals-overlap? iv1 iv2))
  (check-false (intervals-overlap? iv1 iv3))
  (check-equal? (interval-intersection iv1 iv2) (make-time-interval 1500 2000))
  (check-false (interval-intersection iv1 iv3))
  
  ;; Hull and shift
  (check-equal? (interval-union-hull iv1 iv3) (make-time-interval 1000 4000))
  (check-equal? (interval-shift iv1 500) (make-time-interval 1500 2500)))

(test-case "ISO-8601 formatting and parsing round-trip"
  (define now-ns (current-time-nanos))
  (define iso-str (nanos->iso8601 now-ns))
  (check-pred string? iso-str)
  (define parsed-ns (iso8601->nanos iso-str))
  ;; Within 1 microsecond tolerance due to ISO formatting precision
  (check-true (<= (abs (- now-ns parsed-ns)) 1000)
              "Parsed ISO string should match original within microsecond truncation"))

(test-case "Human readable duration formatting"
  (check-equal? (format-duration-human 450) "450ns")
  (check-equal? (format-duration-human 12500) "12.50µs")
  (check-equal? (format-duration-human 145000000) "145.00ms")
  (check-equal? (format-duration-human 2500000000) "2.50s"))
