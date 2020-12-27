#lang racket/base

;;; tests/unit/test_storage.rkt
;;; Unit test suite for ring buffers, WAL binary persistence and recovery,
;;; multi-dimensional inverted indexing, columnar blocks, and storage engine.

(require rackunit
         racket/file
         racket/list
         racket/set
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/storage/buffer.rkt"
         "../../vernier/storage/wal.rkt"
         "../../vernier/storage/index.rkt"
         "../../vernier/storage/column.rkt"
         "../../vernier/storage/partition.rkt"
         "../../vernier/storage/engine.rkt")

(test-case "Ring buffer concurrent push and batch pop"
  (define rb (make-ring-buffer 16))
  (check-true (ring-buffer-empty? rb))
  (check-false (ring-buffer-full? rb))
  
  (for ([i (in-range 10)])
    (check-true (ring-buffer-push! rb i)))
  
  (check-equal? (ring-buffer-size rb) 10)
  (define batch (ring-buffer-pop-batch! rb 6))
  (check-equal? (length batch) 6)
  (check-equal? batch (list 0 1 2 3 4 5))
  (check-equal? (ring-buffer-size rb) 4))

(test-case "CRC32 computation and verification"
  (define data (string->bytes/utf-8 "123456789"))
  ;; Standard test vector: CRC32 of "123456789" is #xcbf43926
  (check-equal? (crc32 data) #xcbf43926))

(test-case "WAL persistence and crash recovery replay"
  (define tmp-dir (make-temporary-file "wal-test-~a" 'directory))
  (define wal (make-wal-writer tmp-dir 1048576))
  
  (define tid (make-trace-id))
  (define s1 (make-span #:trace-id tid #:span-id (make-span-id) #:name "Op1"
                        #:service-name "svc" #:start-time 100 #:end-time 200))
  (define s2 (make-span #:trace-id tid #:span-id (make-span-id) #:name "Op2"
                        #:service-name "svc" #:start-time 200 #:end-time 300))
  
  (check-true (wal-write-span! wal s1))
  (check-true (wal-write-span! wal s2))
  (wal-close! wal)
  
  ;; Replay from directory
  (define replayed (wal-replay-directory tmp-dir))
  (check-equal? (length replayed) 2)
  (check-equal? (span-name (first replayed)) "Op1")
  (check-equal? (span-name (second replayed)) "Op2")
  (delete-directory/files tmp-dir))

(test-case "Inverted index and multi-attribute search"
  (define idx (make-inverted-index))
  (define tid1 (make-trace-id))
  (define tid2 (make-trace-id))
  
  (define s1 (make-span #:trace-id tid1 #:span-id (make-span-id) #:name "Checkout"
                        #:service-name "web" #:start-time 100 #:end-time 200
                        #:status-code 'ok #:attributes (hasheq 'env "prod" 'user "alice")))
  (define s2 (make-span #:trace-id tid2 #:span-id (make-span-id) #:name "Payment"
                        #:service-name "payments" #:start-time 150 #:end-time 350
                        #:status-code 'error #:attributes (hasheq 'env "prod" 'user "bob")))
  
  (define idx1 (index-add-span (index-add-span idx s1) s2))
  
  (check-equal? (index-query-service idx1 "web") (set tid1))
  (check-equal? (index-query-service idx1 "payments") (set tid2))
  (check-equal? (index-query-status idx1 'error) (set tid2))
  (check-equal? (index-query-tag idx1 'env "prod") (set tid1 tid2))
  (check-equal? (index-query-tag idx1 'user "alice") (set tid1)))

(test-case "Columnar block vector filters"
  (define tid (make-trace-id))
  (define s1 (make-span #:trace-id tid #:span-id (make-span-id) #:name "A" #:service-name "svc"
                        #:start-time 1000 #:end-time 1100)) ;; dur 100
  (define s2 (make-span #:trace-id tid #:span-id (make-span-id) #:name "B" #:service-name "svc"
                        #:start-time 1200 #:end-time 1500)) ;; dur 300
  (define cb (make-column-block (list s1 s2)))
  
  (define matches (column-filter-duration cb 200 #f))
  (check-equal? matches (set tid))
  (define no-matches (column-filter-duration cb 500 #f))
  (check-equal? no-matches (set)))

(test-case "Unified Storage Engine end-to-end integration"
  (define engine (make-storage-engine))
  (define tid (make-trace-id))
  (define root-id (make-span-id))
  (define child-id (make-span-id))
  
  (define s-root (make-span #:trace-id tid #:span-id root-id #:name "RootReq"
                            #:service-name "api-gateway" #:start-time 1000 #:end-time 2000
                            #:status-code 'ok #:attributes (hasheq 'region "us-east")))
  (define s-child (make-span #:trace-id tid #:span-id child-id #:parent-id root-id #:name "DBFetch"
                             #:service-name "postgres" #:start-time 1200 #:end-time 1800
                             #:status-code 'ok #:attributes (hasheq 'region "us-east")))
  
  (storage-write-span! engine s-root)
  (storage-write-span! engine s-child)
  
  ;; Lookup by ID
  (define tr (storage-get-trace engine tid))
  (check-not-false tr)
  (check-equal? (length (trace-record-spans tr)) 2)
  (check-equal? (trace-record-duration tr) 1000)
  
  ;; Search query
  (define search-results (storage-search engine #:service "api-gateway"))
  (check-equal? (length search-results) 1)
  (check-equal? (trace-record-trace-id (car search-results)) tid)
  
  (define stats (storage-stats engine))
  (check-equal? (hash-ref stats 'total_spans_indexed) 2)
  (check-equal? (hash-ref stats 'total_traces) 1)
  (storage-close! engine))
