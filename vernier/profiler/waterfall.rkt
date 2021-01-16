#lang racket/base

;;; vernier/profiler/waterfall.rkt
;;; Chronological waterfall layout generator and ASCII terminal renderer.
;;; Visualizes distributed span timelines, thread lanes, and critical paths.

(require racket/contract
         racket/list
         racket/string
         racket/format
         racket/set
         "../core/time.rkt"
         "../core/span.rkt"
         "../core/trace.rkt"
         "../dag/graph.rkt"
         "../dag/critical_path.rkt")

(provide
 (contract-out
  ;; Waterfall item representation
  [struct waterfall-row ([span-id string?]
                         [parent-id (or/c string? #f)]
                         [depth exact-nonnegative-integer?]
                         [service-name string?]
                         [operation-name string?]
                         [start-nanos exact-integer?]
                         [duration-nanos exact-integer?]
                         [relative-offset-pct real?]
                         [relative-width-pct real?]
                         [critical? boolean?]
                         [error? boolean?])]
  
  [build-waterfall-layout (-> trace-record? (listof waterfall-row?))]
  [render-waterfall-ascii (->* (trace-record?) (#:width exact-positive-integer?) string?)]))

(struct waterfall-row (span-id
                       parent-id
                       depth
                       service-name
                       operation-name
                       start-nanos
                       duration-nanos
                       relative-offset-pct
                       relative-width-pct
                       critical?
                       error?)
  #:transparent)

;; Generates chronological waterfall rows with relative percentage coordinates
(define (build-waterfall-layout tr)
  (define cp-res (analyze-critical-path tr))
  (define crit-ids (list->set (map span-span-id (critical-path-result-critical-spans cp-res))))
  (define g (trace->dag tr))
  
  (define t-start (trace-record-start-time tr))
  (define total-dur (max 1 (trace-record-duration tr)))
  
  (define rows '())
  
  (define (walk-span sid current-depth)
    (define s (trace-find-span tr sid))
    (when s
      (define s-start (span-start-time s))
      (define s-dur (span-duration s))
      (define offset-pct (* 100.0 (/ (max 0 (- s-start t-start)) (* 1.0 total-dur))))
      (define width-pct (max 0.2 (* 100.0 (/ s-dur (* 1.0 total-dur)))))
      (define is-crit (set-member? crit-ids sid))
      (define is-err (span-error? s))
      
      (set! rows (cons (waterfall-row sid
                                      (span-parent-id s)
                                      current-depth
                                      (span-service-name s)
                                      (span-name s)
                                      s-start
                                      s-dur
                                      offset-pct
                                      width-pct
                                      is-crit
                                      is-err)
                       rows))
      
      ;; Recurse on children in chronological order
      (define child-ids (hash-ref (trace-dag-adj-out g) sid '()))
      (define child-spans
        (sort (map (lambda (cid) (hash-ref (trace-dag-nodes g) cid)) child-ids)
              < #:key span-start-time))
      (for ([c (in-list child-spans)])
        (walk-span (span-span-id c) (add1 current-depth)))))
  
  (for ([r (in-list (trace-find-roots tr))])
    (walk-span (span-span-id r) 0))
  
  (reverse rows))

;; Renders an ASCII waterfall chart suitable for terminal output
(define (render-waterfall-ascii tr #:width [bar-cols 50])
  (define rows (build-waterfall-layout tr))
  (define lines '())
  
  (define header
    (format "TRACE ~a [~a, ~a spans, ~a]\n~a"
            (trace-record-trace-id tr)
            (format-duration-human (trace-record-duration tr))
            (length (trace-record-spans tr))
            (if (trace-has-errors? tr) "ERROR" "OK")
            (make-string 90 #\=)))
  (set! lines (cons header lines))
  
  (for ([row (in-list rows)])
    (define indent (make-string (* (waterfall-row-depth row) 2) #\space))
    (define svc-op (format "~a~a: ~a" indent (waterfall-row-service-name row) (waterfall-row-operation-name row)))
    (define label-padded (~a svc-op #:min-width 36 #:max-width 36))
    
    (define offset-col (inexact->exact (round (* (/ (waterfall-row-relative-offset-pct row) 100.0) bar-cols))))
    (define width-col (max 1 (inexact->exact (round (* (/ (waterfall-row-relative-width-pct row) 100.0) bar-cols)))))
    (define space-before (make-string (min bar-cols offset-col) #\space))
    (define bar-char
      (cond
        [(waterfall-row-error? row) #\!]
        [(waterfall-row-critical? row) #\#]
        [else #\=]))
    (define bar (make-string width-col bar-char))
    (define dur-str (format-duration-human (waterfall-row-duration-nanos row)))
    
    (define line (format "~a |~a~a| ~a~a"
                         label-padded
                         space-before
                         bar
                         dur-str
                         (if (waterfall-row-critical? row) " [CRITICAL]" "")))
    (set! lines (cons line lines)))
  
  (string-join (reverse lines) "\n"))
