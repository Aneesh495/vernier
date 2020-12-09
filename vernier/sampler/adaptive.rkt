#lang racket/base

;;; vernier/sampler/adaptive.rkt
;;; Dynamic feedback rate adaptation for high-volume background telemetry.
;;; Automatically throttles background sampling during traffic spikes.

(require racket/contract
         racket/math
         "rules.rkt")

(provide
 (contract-out
  ;; Adaptive rate controller
  [struct adaptive-controller ([target-traces-per-sec real?]
                               [current-rate real?]
                               [ema-arrival-rate real?]
                               [last-adjustment-secs exact-integer?]
                               [smoothing-factor real?])]
  
  [make-adaptive-controller (->* (real?) (real? real?) adaptive-controller?)]
  [adaptive-record-throughput (-> adaptive-controller? exact-nonnegative-integer? exact-integer? adaptive-controller?)]
  [adaptive-tune-policy (-> adaptive-controller? sampling-policy? sampling-policy?)]))

(struct adaptive-controller (target-traces-per-sec
                             current-rate
                             ema-arrival-rate
                             last-adjustment-secs
                             smoothing-factor)
  #:transparent)

;; Constructs controller with target kept traces per second (e.g. 100.0)
(define (make-adaptive-controller target-tps [initial-rate 0.10] [smoothing 0.20])
  (adaptive-controller target-tps
                       initial-rate
                       target-tps
                       (current-seconds)
                       smoothing))

;; Records observed trace arrivals over an interval and updates EMA
(define (adaptive-record-throughput ctrl count-in-interval now-secs)
  (define dt (max 1 (- now-secs (adaptive-controller-last-adjustment-secs ctrl))))
  (define instant-rate (/ count-in-interval (* 1.0 dt)))
  (define alpha (adaptive-controller-smoothing-factor ctrl))
  (define old-ema (adaptive-controller-ema-arrival-rate ctrl))
  (define new-ema (+ (* alpha instant-rate) (* (- 1.0 alpha) old-ema)))
  
  (define target (adaptive-controller-target-traces-per-sec ctrl))
  ;; Compute proportional adjustment
  (define new-rate
    (if (zero? new-ema)
        1.0
        (min 1.0 (max 0.001 (/ target new-ema)))))
  
  (adaptive-controller target
                       new-rate
                       new-ema
                       now-secs
                       alpha))

;; Updates policy default background rate with the controller's current rate
(define (adaptive-tune-policy ctrl policy)
  (define tuned-rate (adaptive-controller-current-rate ctrl))
  (struct-copy sampling-policy policy
               [default-rate tuned-rate]))
