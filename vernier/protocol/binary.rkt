#lang racket/base

;;; vernier/protocol/binary.rkt
;;; High-performance compact binary wire serialization for telemetry spans.
;;; Minimizes bandwidth and allocation overhead in high-throughput network transport.

(require racket/contract
         racket/format
         "../core/id.rkt"
         "../core/span.rkt")

(provide
 (contract-out
  [span->binary-bytes (-> span? bytes?)]
  [binary-bytes->span (-> bytes? span?)]))

;; Converts span kind to 1-byte code
(define (kind->byte k)
  (case k
    [(internal) 0]
    [(server) 1]
    [(client) 2]
    [(producer) 3]
    [(consumer) 4]
    [else 0]))

(define (byte->kind b)
  (case b
    [(0) 'internal]
    [(1) 'server]
    [(2) 'client]
    [(3) 'producer]
    [(4) 'consumer]
    [else 'internal]))

(define (status->byte s)
  (case s
    [(unset) 0]
    [(ok) 1]
    [(error) 2]
    [else 0]))

(define (byte->status b)
  (case b
    [(0) 'unset]
    [(1) 'ok]
    [(2) 'error]
    [else 'unset]))

;; Serializes a span into compact binary bytes
(define (span->binary-bytes s)
  (define tid-bytes (trace-id->bytes (span-trace-id s)))
  (define sid-bytes (span-id->bytes (span-span-id s)))
  (define pid (span-parent-id s))
  (define has-pid (if pid 1 0))
  (define pid-bytes (if pid (span-id->bytes pid) (make-bytes 8 0)))
  
  (define start-bytes (integer->integer-bytes (span-start-time s) 8 #f #t))
  (define dur-bytes (integer->integer-bytes (span-duration s) 8 #f #t))
  
  (define svc-bytes (string->bytes/utf-8 (span-service-name s)))
  (define svc-len (integer->integer-bytes (bytes-length svc-bytes) 2 #f #t))
  
  (define name-bytes (string->bytes/utf-8 (span-name s)))
  (define name-len (integer->integer-bytes (bytes-length name-bytes) 2 #f #t))
  
  (bytes-append tid-bytes
                sid-bytes
                (bytes has-pid)
                pid-bytes
                start-bytes
                dur-bytes
                (bytes (kind->byte (span-kind s)))
                (bytes (status->byte (span-status-code s)))
                svc-len
                svc-bytes
                name-len
                name-bytes))

;; Deserializes compact binary bytes back into a span
(define (binary-bytes->span bs)
  (define tid-bytes (subbytes bs 0 16))
  (define sid-bytes (subbytes bs 16 24))
  (define has-pid (bytes-ref bs 24))
  (define pid-bytes (subbytes bs 25 33))
  (define start-ns (integer-bytes->integer (subbytes bs 33 41) #f #t))
  (define dur-ns (integer-bytes->integer (subbytes bs 41 49) #f #t))
  (define kind (byte->kind (bytes-ref bs 49)))
  (define status (byte->status (bytes-ref bs 50)))
  
  (define svc-len (integer-bytes->integer (subbytes bs 51 53) #f #t))
  (define svc-end (+ 53 svc-len))
  (define svc-name (bytes->string/utf-8 (subbytes bs 53 svc-end)))
  
  (define name-len (integer-bytes->integer (subbytes bs svc-end (+ svc-end 2)) #f #t))
  (define name-start (+ svc-end 2))
  (define name (bytes->string/utf-8 (subbytes bs name-start (+ name-start name-len))))
  
  (make-span #:trace-id (bytes->trace-id tid-bytes)
             #:span-id (bytes->span-id sid-bytes)
             #:parent-id (if (= has-pid 1) (bytes->span-id pid-bytes) #f)
             #:name name
             #:service-name svc-name
             #:kind kind
             #:start-time start-ns
             #:end-time (+ start-ns dur-ns)
             #:status-code status))
