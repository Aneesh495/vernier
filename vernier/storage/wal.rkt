#lang racket/base

;;; vernier/storage/wal.rkt
;;; Binary-framed Write-Ahead Log (WAL) with CRC32 integrity verification.
;;; Guarantees crash recoverability and durable telemetry replay.

(require racket/contract
         racket/file
         racket/path
         racket/format
         json
         "../core/span.rkt")

(provide
 (contract-out
  ;; CRC32 helper
  [crc32 (-> bytes? exact-nonnegative-integer?)]
  
  ;; WAL Writer
  [struct wal-writer ([dir path-string?]
                      [segment-index (box/c exact-positive-integer?)]
                      [current-file-bytes (box/c exact-nonnegative-integer?)]
                      [max-segment-bytes exact-positive-integer?]
                      [out-port (box/c (or/c output-port? #f))]
                      [lock semaphore?])]
  
  [make-wal-writer (->* (path-string?) (exact-positive-integer?) wal-writer?)]
  [wal-write-span! (-> wal-writer? span? boolean?)]
  [wal-flush! (-> wal-writer? void?)]
  [wal-close! (-> wal-writer? void?)]
  
  ;; WAL Reader and Recovery
  [struct wal-record ([record-type exact-integer?]
                      [payload-bytes bytes?]
                      [valid? boolean?])]
  
  [wal-replay-directory (-> path-string? (listof span?))]))

;; Precomputed CRC32 lookup table using IEEE 802.3 polynomial #xedb88320
(define crc32-table
  (let ([table (make-vector 256 0)])
    (for ([i (in-range 256)])
      (define c
        (for/fold ([crc i]) ([j (in-range 8)])
          (if (not (zero? (bitwise-and crc 1)))
              (bitwise-xor (arithmetic-shift crc -1) #xedb88320)
              (arithmetic-shift crc -1))))
      (vector-set! table i c))
    table))

;; Computes 32-bit CRC checksum over arbitrary byte string
(define (crc32 bs)
  (define MASK32 #xffffffff)
  (define final-crc
    (for/fold ([crc #xffffffff]) ([b (in-bytes bs)])
      (define idx (bitwise-and (bitwise-xor crc b) #xff))
      (bitwise-xor (arithmetic-shift crc -8) (vector-ref crc32-table idx))))
  (bitwise-and (bitwise-xor final-crc #xffffffff) MASK32))

(struct wal-writer (dir segment-index current-file-bytes max-segment-bytes out-port lock)
  #:transparent)

(struct wal-record (record-type payload-bytes valid?)
  #:transparent)

(define WAL-MAGIC #"VERN")
(define WAL-VERSION 1)
(define RECORD-TYPE-SPAN 1)

;; Formats segment filename: 000001.wal
(define (segment-filename dir idx)
  (build-path dir (format "~a.wal" (~r idx #:min-width 6 #:pad-string "0"))))

;; Opens a new segment with header
(define (open-segment-file! w)
  (make-directory* (wal-writer-dir w))
  (define fpath (segment-filename (wal-writer-dir w) (unbox (wal-writer-segment-index w))))
  (define p (open-output-file fpath #:mode 'binary #:exists 'append))
  (when (zero? (file-size fpath))
    (write-bytes WAL-MAGIC p)
    (write-byte WAL-VERSION p)
    (write-bytes (make-bytes 3 0) p) ;; 3 reserved bytes
    (flush-output p)
    (set-box! (wal-writer-current-file-bytes w) 8))
  (set-box! (wal-writer-out-port w) p))

;; Constructs a WAL writer
(define (make-wal-writer dir [max-segment-bytes 10485760]) ;; Default 10 MB per segment
  (define w (wal-writer (expand-user-path dir)
                        (box 1)
                        (box 0)
                        max-segment-bytes
                        (box #f)
                        (make-semaphore 1)))
  (open-segment-file! w)
  w)

;; Writes a span to the active WAL segment
(define (wal-write-span! w s)
  (call-with-semaphore
   (wal-writer-lock w)
   (lambda ()
     ;; Rotate segment if file exceeded max size
     (when (>= (unbox (wal-writer-current-file-bytes w)) (wal-writer-max-segment-bytes w))
       (when (unbox (wal-writer-out-port w))
         (close-output-port (unbox (wal-writer-out-port w))))
       (set-box! (wal-writer-segment-index w) (add1 (unbox (wal-writer-segment-index w))))
       (set-box! (wal-writer-current-file-bytes w) 0)
       (open-segment-file! w))
     
     (define payload-bytes (string->bytes/utf-8 (jsexpr->string (span->hash s))))
     (define payload-len (bytes-length payload-bytes))
     (define type-byte (integer->integer-bytes RECORD-TYPE-SPAN 1 #f #t))
     (define data-for-crc (bytes-append type-byte payload-bytes))
     (define checksum (crc32 data-for-crc))
     
     (define p (unbox (wal-writer-out-port w)))
     (write-bytes (integer->integer-bytes payload-len 4 #f #t) p)
     (write-byte RECORD-TYPE-SPAN p)
     (write-bytes (integer->integer-bytes checksum 4 #f #t) p)
     (write-bytes payload-bytes p)
     (flush-output p)
     
     (define total-written (+ 4 1 4 payload-len))
     (set-box! (wal-writer-current-file-bytes w)
               (+ (unbox (wal-writer-current-file-bytes w)) total-written))
     #t)))

(define (wal-flush! w)
  (call-with-semaphore
   (wal-writer-lock w)
   (lambda ()
     (define p (unbox (wal-writer-out-port w)))
     (when p (flush-output p)))))

(define (wal-close! w)
  (call-with-semaphore
   (wal-writer-lock w)
   (lambda ()
     (define p (unbox (wal-writer-out-port w)))
     (when p
       (flush-output p)
       (close-output-port p)
       (set-box! (wal-writer-out-port w) #f)))))

;; Reads a single record from port, returns (values span #t) or (values #f #f) on EOF/corruption
(define (read-wal-record in)
  (define len-bytes (read-bytes 4 in))
  (if (or (eof-object? len-bytes) (< (bytes-length len-bytes) 4))
      (values #f #f)
      (let* ([payload-len (integer-bytes->integer len-bytes #f #t)]
             [type-byte (read-byte in)]
             [crc-bytes (read-bytes 4 in)])
        (if (or (eof-object? type-byte)
                (eof-object? crc-bytes)
                (< (bytes-length crc-bytes) 4))
            (values #f #f)
            (let* ([expected-crc (integer-bytes->integer crc-bytes #f #t)]
                   [payload (read-bytes payload-len in)])
              (if (or (eof-object? payload) (< (bytes-length payload) payload-len))
                  (values #f #f)
                  (let* ([data-for-crc (bytes-append (bytes type-byte) payload)]
                         [computed-crc (crc32 data-for-crc)])
                    (if (= expected-crc computed-crc)
                        (let* ([h (string->jsexpr (bytes->string/utf-8 payload))]
                               [parsed-span (hash->span h)])
                          (values parsed-span #t))
                        (values #f #f)))))))))

;; Replays and validates an individual WAL segment file
(define (replay-segment-file fpath)
  (if (not (file-exists? fpath))
      '()
      (call-with-input-file fpath
        (lambda (in)
          (define magic (read-bytes 4 in))
          (if (not (equal? magic WAL-MAGIC))
              '()
              (begin
                (read-byte in)    ;; version
                (read-bytes 3 in) ;; reserved
                (let loop ([spans '()])
                  (let-values ([(s valid?) (read-wal-record in)])
                    (if valid?
                        (loop (cons s spans))
                        (reverse spans))))))))))

;; Replays all WAL segment files in directory in sequential numerical order
(define (wal-replay-directory dir)
  (if (not (directory-exists? dir))
      '()
      (let* ([files (directory-list dir #:build? #t)]
             [wal-files (filter (lambda (p) (path-has-extension? p #".wal")) files)]
             [sorted-files (sort wal-files string<? #:key path->string)])
        (apply append (map replay-segment-file sorted-files)))))
