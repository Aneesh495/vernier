#lang racket/base

;;; vernier/sketch/hll.rkt
;;; HyperLogLog streaming distinct cardinality estimator with linear counting fallback.
;;; Accurately counts distinct services, client IPs, and trace IDs in constant memory.

(require racket/contract
         racket/math
         racket/vector)

(provide
 (contract-out
  ;; HyperLogLog struct
  [struct hyperloglog ([p (integer-in 4 16)]
                       [m exact-positive-integer?]
                       [alpha real?]
                       [registers bytes?])]
  
  [make-hyperloglog (->* () ((integer-in 4 16)) hyperloglog?)]
  [hll-insert (-> hyperloglog? (or/c string? bytes? exact-integer?) hyperloglog?)]
  [hll-insert! (-> (box/c hyperloglog?) (or/c string? bytes? exact-integer?) void?)]
  [hll-cardinality (-> hyperloglog? exact-nonnegative-integer?)]
  [hll-merge (-> hyperloglog? hyperloglog? hyperloglog?)]))

(struct hyperloglog (p m alpha registers)
  #:transparent)

;; Precomputed alpha_m constants
(define (compute-alpha m)
  (case m
    [(16) 0.673]
    [(32) 0.697]
    [(64) 0.709]
    [else (/ 0.7213 (+ 1.0 (/ 1.079 m)))]))

;; Constructs a new HyperLogLog with precision p (default 10 -> 1024 registers, ~3.25% standard error)
(define (make-hyperloglog [p 10])
  (define m (arithmetic-shift 1 p))
  (define alpha (compute-alpha m))
  (define regs (make-bytes m 0))
  (hyperloglog p m alpha regs))

;; MurmurHash3 64-bit finalizer / avalanche mixer
(define (mix64 k)
  (define MASK64 #xffffffffffffffff)
  (define k1 (bitwise-xor k (arithmetic-shift k -33)))
  (define k2 (bitwise-and (* k1 #xff51afd7ed558ccd) MASK64))
  (define k3 (bitwise-xor k2 (arithmetic-shift k2 -33)))
  (define k4 (bitwise-and (* k3 #xc4ceb9fe1a85ec53) MASK64))
  (bitwise-xor k4 (arithmetic-shift k4 -33)))

;; FNV-1a 64-bit hash function followed by avalanche mixing
(define (fnv1a-64 data)
  (define bs
    (cond
      [(bytes? data) data]
      [(string? data) (string->bytes/utf-8 data)]
      [(exact-integer? data) (string->bytes/utf-8 (number->string data))]
      [else (string->bytes/utf-8 (format "~a" data))]))
  
  (define FNV-PRIME #x100000001b3)
  (define FNV-OFFSET #xcbf29ce484222325)
  (define MASK64 #xffffffffffffffff)
  
  (define raw-hash
    (for/fold ([hash FNV-OFFSET])
              ([b (in-bytes bs)])
      (define xored (bitwise-xor hash b))
      (bitwise-and (* xored FNV-PRIME) MASK64)))
  
  (mix64 raw-hash))

;; Count leading zeros in lower 64-p bits + 1
(define (count-leading-zeros val bits-count)
  (let loop ([bit (sub1 bits-count)] [zeros 0])
    (cond
      [(< bit 0) (add1 bits-count)]
      [(zero? (bitwise-and val (arithmetic-shift 1 bit)))
       (loop (sub1 bit) (add1 zeros))]
      [else (add1 zeros)])))

;; Functional insertion into HyperLogLog
(define (hll-insert h item)
  (define hash-val (fnv1a-64 item))
  (define p (hyperloglog-p h))
  (define m (hyperloglog-m h))
  (define reg-idx (bitwise-and (arithmetic-shift hash-val (- p 64)) (sub1 m)))
  (define w (bitwise-and hash-val (sub1 (arithmetic-shift 1 (- 64 p)))))
  (define leading-zeros (count-leading-zeros w (- 64 p)))
  
  (define current-val (bytes-ref (hyperloglog-registers h) reg-idx))
  (if (> leading-zeros current-val)
      (let ([new-regs (bytes-copy (hyperloglog-registers h))])
        (bytes-set! new-regs reg-idx (min 255 leading-zeros))
        (struct-copy hyperloglog h [registers new-regs]))
      h))

(define (hll-insert! box-h item)
  (set-box! box-h (hll-insert (unbox box-h) item)))

;; Calculates estimated cardinality with small-range correction
(define (hll-cardinality h)
  (define m (hyperloglog-m h))
  (define regs (hyperloglog-registers h))
  (define-values (z zeros-count)
    (for/fold ([sum-z 0.0] [zeros 0])
              ([b (in-bytes regs)])
      (values (+ sum-z (expt 2.0 (- b)))
              (if (zero? b) (add1 zeros) zeros))))
  
  (define raw-estimate (* (hyperloglog-alpha h) (* m m) (/ 1.0 z)))
  (cond
    ;; Linear counting correction for small cardinalities
    [(and (<= raw-estimate (* 2.5 m)) (> zeros-count 0))
     (exact-round (* m (log (/ (* 1.0 m) zeros-count))))]
    ;; Large range correction for 64-bit hash
    [(> raw-estimate (* (/ 1.0 30.0) (expt 2.0 64)))
     (exact-round (* (- (expt 2.0 64)) (log (- 1.0 (/ raw-estimate (expt 2.0 64))))))]
    [else
     (exact-round raw-estimate)]))

;; Merges two HyperLogLog estimators of the same precision
(define (hll-merge h1 h2)
  (unless (= (hyperloglog-p h1) (hyperloglog-p h2))
    (error 'hll-merge "Cannot merge HLL with differing precision: ~a vs ~a"
           (hyperloglog-p h1) (hyperloglog-p h2)))
  (define m (hyperloglog-m h1))
  (define merged (make-bytes m 0))
  (define r1 (hyperloglog-registers h1))
  (define r2 (hyperloglog-registers h2))
  (for ([i (in-range m)])
    (bytes-set! merged i (max (bytes-ref r1 i) (bytes-ref r2 i))))
  (struct-copy hyperloglog h1 [registers merged]))
