#lang racket/base

;;; vernier/sketch/bloom.rkt
;;; Space-efficient Bloom filter using Kirsch-Mitzenmacher double-hashing.
;;; Provides constant-time probabilistic membership testing for tag indexing.

(require racket/contract
         racket/math)

(provide
 (contract-out
  ;; Bloom filter struct
  [struct bloom-filter ([size exact-positive-integer?]
                        [num-hashes exact-positive-integer?]
                        [count exact-nonnegative-integer?]
                        [bits bytes?])]
  
  [make-bloom-filter (->* (exact-positive-integer?) (real?) bloom-filter?)]
  [bloom-insert (-> bloom-filter? (or/c string? bytes?) bloom-filter?)]
  [bloom-insert! (-> (box/c bloom-filter?) (or/c string? bytes?) void?)]
  [bloom-contains? (-> bloom-filter? (or/c string? bytes?) boolean?)]
  [bloom-estimated-fpp (-> bloom-filter? real?)]
  [bloom-union (-> bloom-filter? bloom-filter? bloom-filter?)]
  [bloom-intersection (-> bloom-filter? bloom-filter? bloom-filter?)]))

(struct bloom-filter (size num-hashes count bits)
  #:transparent)

;; Double-hash helper using FNV and Murmur-style mixer
(define (hash-pair key)
  (define bs (if (bytes? key) key (string->bytes/utf-8 key)))
  (define h1
    (for/fold ([h #xcbf29ce484222325]) ([b (in-bytes bs)])
      (bitwise-and (* (bitwise-xor h b) #x100000001b3) #xffffffffffffffff)))
  (define h2
    (for/fold ([h #x84222325cbf29ce4]) ([b (in-bytes bs)])
      (bitwise-and (* (bitwise-xor h b) #x2545f4914f6cdd1d) #xffffffffffffffff)))
  (values h1 (if (zero? h2) 1 h2)))

;; Constructs a Bloom filter dimensioned for expected-items and false-positive probability
(define (make-bloom-filter expected-items [fpp 0.01])
  (define ln2 0.6931471805599453)
  (define m (exact-ceiling (/ (* (- expected-items) (log fpp)) (* ln2 ln2))))
  (define k (max 1 (exact-round (* (/ m expected-items) ln2))))
  (define byte-len (exact-ceiling (/ m 8.0)))
  (bloom-filter m k 0 (make-bytes byte-len 0)))

;; Computes the bit index for the i-th hash function
(define (nth-bit-index h1 h2 i m)
  (modulo (+ h1 (* i h2)) m))

;; Functional insert of key into Bloom filter
(define (bloom-insert bf key)
  (define m (bloom-filter-size bf))
  (define k (bloom-filter-num-hashes bf))
  (define-values (h1 h2) (hash-pair key))
  (define new-bits (bytes-copy (bloom-filter-bits bf)))
  
  (for ([i (in-range k)])
    (define bit-idx (nth-bit-index h1 h2 i m))
    (define byte-pos (quotient bit-idx 8))
    (define bit-pos (remainder bit-idx 8))
    (define old-byte (bytes-ref new-bits byte-pos))
    (bytes-set! new-bits byte-pos (bitwise-ior old-byte (arithmetic-shift 1 bit-pos))))
  
  (struct-copy bloom-filter bf
               [count (add1 (bloom-filter-count bf))]
               [bits new-bits]))

(define (bloom-insert! box-bf key)
  (set-box! box-bf (bloom-insert (unbox box-bf) key)))

;; Tests whether key is potentially in the Bloom filter
(define (bloom-contains? bf key)
  (define m (bloom-filter-size bf))
  (define k (bloom-filter-num-hashes bf))
  (define-values (h1 h2) (hash-pair key))
  (define raw-bits (bloom-filter-bits bf))
  
  (for/and ([i (in-range k)])
    (define bit-idx (nth-bit-index h1 h2 i m))
    (define byte-pos (quotient bit-idx 8))
    (define bit-pos (remainder bit-idx 8))
    (define byte-val (bytes-ref raw-bits byte-pos))
    (not (zero? (bitwise-and byte-val (arithmetic-shift 1 bit-pos))))))

;; Computes current theoretical false-positive probability
(define (bloom-estimated-fpp bf)
  (define m (bloom-filter-size bf))
  (define k (bloom-filter-num-hashes bf))
  (define n (bloom-filter-count bf))
  (expt (- 1.0 (exp (/ (- (* k n)) (* 1.0 m)))) k))

;; Bitwise union (OR) of two compatible Bloom filters
(define (bloom-union bf1 bf2)
  (unless (and (= (bloom-filter-size bf1) (bloom-filter-size bf2))
               (= (bloom-filter-num-hashes bf1) (bloom-filter-num-hashes bf2)))
    (error 'bloom-union "Bloom filters have incompatible dimensions"))
  (define b1 (bloom-filter-bits bf1))
  (define b2 (bloom-filter-bits bf2))
  (define len (bytes-length b1))
  (define out (make-bytes len 0))
  (for ([i (in-range len)])
    (bytes-set! out i (bitwise-ior (bytes-ref b1 i) (bytes-ref b2 i))))
  (bloom-filter (bloom-filter-size bf1)
                (bloom-filter-num-hashes bf1)
                (+ (bloom-filter-count bf1) (bloom-filter-count bf2))
                out))

;; Bitwise intersection (AND) of two compatible Bloom filters
(define (bloom-intersection bf1 bf2)
  (unless (and (= (bloom-filter-size bf1) (bloom-filter-size bf2))
               (= (bloom-filter-num-hashes bf1) (bloom-filter-num-hashes bf2)))
    (error 'bloom-intersection "Bloom filters have incompatible dimensions"))
  (define b1 (bloom-filter-bits bf1))
  (define b2 (bloom-filter-bits bf2))
  (define len (bytes-length b1))
  (define out (make-bytes len 0))
  (for ([i (in-range len)])
    (bytes-set! out i (bitwise-and (bytes-ref b1 i) (bytes-ref b2 i))))
  (bloom-filter (bloom-filter-size bf1)
                (bloom-filter-num-hashes bf1)
                (min (bloom-filter-count bf1) (bloom-filter-count bf2))
                out))
