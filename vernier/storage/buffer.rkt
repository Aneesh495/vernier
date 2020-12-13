#lang racket/base

;;; vernier/storage/buffer.rkt
;;; In-memory thread-safe circular ring buffer for high-throughput span ingestion.
;;; Provides bounded buffering between ingestion network threads and storage workers.

(require racket/contract
         racket/vector)

(provide
 (contract-out
  ;; Ring buffer structure
  [struct ring-buffer ([capacity exact-positive-integer?]
                       [mask exact-positive-integer?]
                       [head-box (box/c exact-nonnegative-integer?)]
                       [tail-box (box/c exact-nonnegative-integer?)]
                       [buffer vector?]
                       [lock semaphore?])]
  
  [make-ring-buffer (->* () (exact-positive-integer?) ring-buffer?)]
  [ring-buffer-push! (-> ring-buffer? any/c boolean?)]
  [ring-buffer-pop! (-> ring-buffer? (or/c any/c #f))]
  [ring-buffer-pop-batch! (-> ring-buffer? exact-positive-integer? list?)]
  [ring-buffer-size (-> ring-buffer? exact-nonnegative-integer?)]
  [ring-buffer-full? (-> ring-buffer? boolean?)]
  [ring-buffer-empty? (-> ring-buffer? boolean?)]))

(struct ring-buffer (capacity mask head-box tail-box buffer lock)
  #:transparent)

;; Constructs a ring buffer with power-of-two capacity for fast bitwise masking
(define (make-ring-buffer [requested-cap 65536])
  ;; Round up to power of 2
  (define cap
    (let loop ([c 1])
      (if (>= c requested-cap) c (loop (* c 2)))))
  (define mask (sub1 cap))
  (ring-buffer cap
               mask
               (box 0)
               (box 0)
               (make-vector cap #f)
               (make-semaphore 1)))

;; Thread-safe push into ring buffer. Returns #f if buffer is full.
(define (ring-buffer-push! rb item)
  (call-with-semaphore
   (ring-buffer-lock rb)
   (lambda ()
     (define head (unbox (ring-buffer-head-box rb)))
     (define tail (unbox (ring-buffer-tail-box rb)))
     (define count (- head tail))
     (if (>= count (ring-buffer-capacity rb))
         #f ;; Buffer full
         (let ([idx (bitwise-and head (ring-buffer-mask rb))])
           (vector-set! (ring-buffer-buffer rb) idx item)
           (set-box! (ring-buffer-head-box rb) (add1 head))
           #t)))))

;; Thread-safe pop of single item. Returns #f if empty.
(define (ring-buffer-pop! rb)
  (call-with-semaphore
   (ring-buffer-lock rb)
   (lambda ()
     (define head (unbox (ring-buffer-head-box rb)))
     (define tail (unbox (ring-buffer-tail-box rb)))
     (if (= head tail)
         #f ;; Buffer empty
         (let* ([idx (bitwise-and tail (ring-buffer-mask rb))]
                [item (vector-ref (ring-buffer-buffer rb) idx)])
           (vector-set! (ring-buffer-buffer rb) idx #f) ;; Avoid retention leak
           (set-box! (ring-buffer-tail-box rb) (add1 tail))
           item)))))

;; Pops up to max-count items in a single batch
(define (ring-buffer-pop-batch! rb max-count)
  (call-with-semaphore
   (ring-buffer-lock rb)
   (lambda ()
     (define head (unbox (ring-buffer-head-box rb)))
     (define tail (unbox (ring-buffer-tail-box rb)))
     (define available (- head tail))
     (define batch-size (min available max-count))
     (if (zero? batch-size)
         '()
         (let ([items
                (for/list ([i (in-range batch-size)])
                  (define idx (bitwise-and (+ tail i) (ring-buffer-mask rb)))
                  (define itm (vector-ref (ring-buffer-buffer rb) idx))
                  (vector-set! (ring-buffer-buffer rb) idx #f)
                  itm)])
           (set-box! (ring-buffer-tail-box rb) (+ tail batch-size))
           items)))))

(define (ring-buffer-size rb)
  (call-with-semaphore
   (ring-buffer-lock rb)
   (lambda ()
     (- (unbox (ring-buffer-head-box rb))
        (unbox (ring-buffer-tail-box rb))))))

(define (ring-buffer-full? rb)
  (>= (ring-buffer-size rb) (ring-buffer-capacity rb)))

(define (ring-buffer-empty? rb)
  (zero? (ring-buffer-size rb)))
