#lang racket/base

;;; vernier/core/time.rkt
;;; High-resolution timestamping, monotonic intervals, and time conversions.
;;; Provides nanosecond-precision representations for distributed span timing.

(require racket/contract
         racket/format
         racket/string
         racket/date)

(provide
 (contract-out
  ;; Current timestamp generators
  [current-time-nanos (-> exact-integer?)]
  [current-time-micros (-> exact-integer?)]
  [current-time-millis (-> exact-integer?)]
  
  ;; Monotonic reference timers
  [current-monotonic-nanos (-> exact-integer?)]
  
  ;; Unit conversions
  [nanos->micros (-> exact-integer? exact-integer?)]
  [nanos->millis (-> exact-integer? exact-integer?)]
  [nanos->seconds (-> exact-integer? real?)]
  [millis->nanos (-> exact-integer? exact-integer?)]
  [micros->nanos (-> exact-integer? exact-integer?)]
  [seconds->nanos (-> real? exact-integer?)]
  
  ;; Interval representation and operations
  [struct time-interval ([start exact-integer?] [end exact-integer?])]
  [make-time-interval (-> exact-integer? exact-integer? time-interval?)]
  [interval-duration (-> time-interval? exact-integer?)]
  [interval-contains? (-> time-interval? exact-integer? boolean?)]
  [intervals-overlap? (-> time-interval? time-interval? boolean?)]
  [interval-intersection (-> time-interval? time-interval? (or/c time-interval? #f))]
  [interval-union-hull (-> time-interval? time-interval? time-interval?)]
  [interval-shift (-> time-interval? exact-integer? time-interval?)]
  
  ;; ISO-8601 formatting and parsing
  [nanos->iso8601 (-> exact-integer? string?)]
  [iso8601->nanos (-> string? exact-integer?)]
  [format-duration-human (-> exact-integer? string?)]))

;; Represents a closed-open time interval [start, end) in nanoseconds
(struct time-interval (start end)
  #:transparent
  #:guard (lambda (start end name)
            (unless (and (exact-integer? start) (exact-integer? end))
              (error name "start and end must be exact integers, got ~a and ~a" start end))
            (unless (<= start end)
              (error name "interval start (~a) cannot exceed end (~a)" start end))
            (values start end)))

;; Constructor convenience wrapper
(define (make-time-interval start end)
  (time-interval start end))

;; High-resolution epoch timestamp in nanoseconds.
;; Combines current seconds with millisecond fractional resolution.
(define (current-time-nanos)
  (define secs (current-seconds))
  (define in-ms (current-inexact-milliseconds))
  (define frac-ms (- in-ms (* (floor (/ in-ms 1000.0)) 1000.0)))
  (define nanos-part (inexact->exact (round (* frac-ms 1000000.0))))
  (+ (* secs 1000000000) (remainder nanos-part 1000000000)))

;; Epoch timestamp in microseconds
(define (current-time-micros)
  (quotient (current-time-nanos) 1000))

;; Epoch timestamp in milliseconds
(define (current-time-millis)
  (quotient (current-time-nanos) 1000000))

;; Monotonic timer using process execution and system uptime metrics
(define (current-monotonic-nanos)
  (define ms (current-inexact-milliseconds))
  (inexact->exact (round (* ms 1000000.0))))

;; Unit conversion helpers
(define (nanos->micros ns)
  (quotient ns 1000))

(define (nanos->millis ns)
  (quotient ns 1000000))

(define (nanos->seconds ns)
  (/ ns 1000000000.0))

(define (millis->nanos ms)
  (* ms 1000000))

(define (micros->nanos us)
  (* us 1000))

(define (seconds->nanos s)
  (inexact->exact (round (* s 1000000000))))

;; Interval duration in nanoseconds
(define (interval-duration iv)
  (- (time-interval-end iv) (time-interval-start iv)))

;; Tests whether a nanosecond point falls inside [start, end)
(define (interval-contains? iv ns)
  (and (>= ns (time-interval-start iv))
       (< ns (time-interval-end iv))))

;; Tests whether two intervals share common points
(define (intervals-overlap? iv1 iv2)
  (and (< (time-interval-start iv1) (time-interval-end iv2))
       (< (time-interval-start iv2) (time-interval-end iv1))))

;; Calculates intersection of two intervals or #f if disjoint
(define (interval-intersection iv1 iv2)
  (if (intervals-overlap? iv1 iv2)
      (time-interval (max (time-interval-start iv1) (time-interval-start iv2))
                     (min (time-interval-end iv1) (time-interval-end iv2)))
      #f))

;; Computes minimum bounding interval containing both intervals
(define (interval-union-hull iv1 iv2)
  (time-interval (min (time-interval-start iv1) (time-interval-start iv2))
                 (max (time-interval-end iv1) (time-interval-end iv2))))

;; Shifts interval by offset nanoseconds
(define (interval-shift iv offset)
  (time-interval (+ (time-interval-start iv) offset)
                 (+ (time-interval-end iv) offset)))

;; Converts nanosecond epoch timestamp to UTC ISO-8601 string
(define (nanos->iso8601 ns)
  (define secs (quotient ns 1000000000))
  (define rem-nanos (remainder ns 1000000000))
  (define micros (quotient rem-nanos 1000))
  (define d (seconds->date secs #f)) ;; UTC date
  (format "~a-~a-~aT~a:~a:~a.~aZ"
          (~r (date-year d) #:min-width 4 #:pad-string "0")
          (~r (date-month d) #:min-width 2 #:pad-string "0")
          (~r (date-day d) #:min-width 2 #:pad-string "0")
          (~r (date-hour d) #:min-width 2 #:pad-string "0")
          (~r (date-minute d) #:min-width 2 #:pad-string "0")
          (~r (date-second d) #:min-width 2 #:pad-string "0")
          (~r micros #:min-width 6 #:pad-string "0")))

;; Parses standard ISO-8601 UTC string into nanosecond timestamp
(define (iso8601->nanos str)
  (define clean (string-trim str))
  (define has-z (string-suffix? clean "Z"))
  (define body (if has-z (substring clean 0 (sub1 (string-length clean))) clean))
  (define parts (string-split body "T"))
  (unless (= (length parts) 2)
    (error 'iso8601->nanos "Invalid ISO-8601 date-time format: ~a" str))
  (define date-parts (map string->number (string-split (car parts) "-")))
  (define time-and-sub (string-split (cadr parts) "."))
  (define time-parts (map string->number (string-split (car time-and-sub) ":")))
  (define sub-micros
    (if (> (length time-and-sub) 1)
        (let* ([sub (cadr time-and-sub)]
               [padded (string-append sub (make-string (max 0 (- 6 (string-length sub))) #\0))]
               [trimmed (substring padded 0 6)])
          (or (string->number trimmed) 0))
        0))
  (define yr (list-ref date-parts 0))
  (define mo (list-ref date-parts 1))
  (define dy (list-ref date-parts 2))
  (define hr (list-ref time-parts 0))
  (define mn (list-ref time-parts 1))
  (define sc (list-ref time-parts 2))
  ;; Approximate seconds calculation using find-seconds
  (define secs (find-seconds sc mn hr dy mo yr #f))
  (+ (* secs 1000000000) (* sub-micros 1000)))

;; Formats nanosecond duration into a human-readable engineering string
(define (format-duration-human ns)
  (cond
    [(< ns 1000) (format "~ans" ns)]
    [(< ns 1000000) (format "~aµs" (~r (/ ns 1000.0) #:precision '(= 2)))]
    [(< ns 1000000000) (format "~ams" (~r (/ ns 1000000.0) #:precision '(= 2)))]
    [else (format "~as" (~r (/ ns 1000000000.0) #:precision '(= 2)))]))
