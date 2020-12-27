#lang racket/base

;;; vernier/storage/engine.rkt
;;; Unified storage engine orchestrating ring buffers, write-ahead logs,
;;; multi-dimensional inverted indexes, and partitioned columnar segments.

(require racket/contract
         racket/set
         racket/list
         "../core/time.rkt"
         "../core/span.rkt"
         "../core/trace.rkt"
         "buffer.rkt"
         "wal.rkt"
         "index.rkt"
         "partition.rkt")

(provide
 (contract-out
  ;; Storage engine structure
  [struct storage-engine ([ring-buf ring-buffer?]
                          [wal-w (or/c wal-writer? #f)]
                          [partition-mgr (box/c partition-manager?)]
                          [global-index (box/c inverted-index?)]
                          [trace-map (box/c hash?)]
                          [span-counter (box/c exact-nonnegative-integer?)]
                          [lock semaphore?])]
  
  [make-storage-engine (->* () (#:wal-dir (or/c path-string? #f)) storage-engine?)]
  [storage-write-span! (-> storage-engine? span? boolean?)]
  [storage-flush! (-> storage-engine? exact-nonnegative-integer?)]
  [storage-get-trace (-> storage-engine? string? (or/c trace-record? #f))]
  [storage-search (->* (storage-engine?)
                       (#:service (or/c string? #f)
                        #:operation (or/c string? #f)
                        #:status (or/c symbol? #f)
                        #:min-duration (or/c exact-integer? #f)
                        #:max-duration (or/c exact-integer? #f)
                        #:start-time (or/c exact-integer? #f)
                        #:end-time (or/c exact-integer? #f)
                        #:tag-key any/c
                        #:tag-val any/c
                        #:limit exact-positive-integer?)
                       (listof trace-record?))]
  [storage-list-services (-> storage-engine? (listof string?))]
  [storage-stats (-> storage-engine? hash?)]
  [storage-close! (-> storage-engine? void?)]))

(struct storage-engine (ring-buf
                        wal-w
                        partition-mgr
                        global-index
                        trace-map
                        span-counter
                        lock)
  #:transparent)

;; Constructs storage engine
(define (make-storage-engine #:wal-dir [wal-dir #f])
  (define wal (and wal-dir (make-wal-writer wal-dir)))
  (storage-engine (make-ring-buffer 32768)
                  wal
                  (box (make-partition-manager))
                  (box (make-inverted-index))
                  (box (hash))
                  (box 0)
                  (make-semaphore 1)))

;; Ingests a span into buffer and WAL
(define (storage-write-span! engine s)
  (when (storage-engine-wal-w engine)
    (wal-write-span! (storage-engine-wal-w engine) s))
  
  (define pushed (ring-buffer-push! (storage-engine-ring-buf engine) s))
  (unless pushed
    ;; Ring buffer was full: trigger synchronous drain
    (storage-flush! engine)
    (ring-buffer-push! (storage-engine-ring-buf engine) s))
  #t)

;; Drains items from ring buffer into partitions and indexes
(define (storage-flush! engine)
  (call-with-semaphore
   (storage-engine-lock engine)
   (lambda ()
     (define batch (ring-buffer-pop-batch! (storage-engine-ring-buf engine) 4096))
     (when (not (null? batch))
       (for ([s (in-list batch)])
         ;; 1. Update partition manager
         (set-box! (storage-engine-partition-mgr engine)
                   (partition-manager-insert (unbox (storage-engine-partition-mgr engine)) s))
         ;; 2. Update global inverted index
         (index-add-span! (storage-engine-global-index engine) s)
         ;; 3. Update active trace map
         (define tid (span-trace-id s))
         (define current-traces (unbox (storage-engine-trace-map engine)))
         (define existing (hash-ref current-traces tid #f))
         (define updated-tr
           (if existing
               (trace-add-span existing s)
               (make-trace (list s))))
         (set-box! (storage-engine-trace-map engine)
                   (hash-set current-traces tid updated-tr))
         ;; 4. Increment counter
         (set-box! (storage-engine-span-counter engine)
                   (add1 (unbox (storage-engine-span-counter engine))))))
     (length batch))))

;; Retrieves complete trace by ID
(define (storage-get-trace engine tid)
  (storage-flush! engine)
  (hash-ref (unbox (storage-engine-trace-map engine)) tid #f))

;; Searches traces with multi-attribute filtering
(define (storage-search engine
                        #:service [svc #f]
                        #:operation [op #f]
                        #:status [stat #f]
                        #:min-duration [min-dur #f]
                        #:max-duration [max-dur #f]
                        #:start-time [t0 #f]
                        #:end-time [t1 #f]
                        #:tag-key [tag-k #f]
                        #:tag-val [tag-v #f]
                        #:limit [limit 50])
  (storage-flush! engine)
  (define idx (unbox (storage-engine-global-index engine)))
  (define traces (unbox (storage-engine-trace-map engine)))
  
  ;; Start with all trace IDs or candidate posting sets
  (define candidate-sets '())
  (when svc
    (set! candidate-sets (cons (index-query-service idx svc) candidate-sets)))
  (when op
    (set! candidate-sets (cons (index-query-operation idx op) candidate-sets)))
  (when stat
    (set! candidate-sets (cons (index-query-status idx stat) candidate-sets)))
  (when (and tag-k tag-v)
    (set! candidate-sets (cons (index-query-tag idx tag-k tag-v) candidate-sets)))
  
  (define matched-tids
    (if (null? candidate-sets)
        (list->set (hash-keys traces))
        (for/fold ([res (car candidate-sets)]) ([s (in-list (cdr candidate-sets))])
          (set-intersect res s))))
  
  ;; Filter by duration and time range
  (define filtered-traces
    (for/list ([tid (in-set matched-tids)]
               #:when (hash-has-key? traces tid)
               #:break (>= (length candidate-sets) limit))
      (hash-ref traces tid)))
  
  (define final-list
    (filter (lambda (tr)
              (and (or (not min-dur) (>= (trace-record-duration tr) min-dur))
                   (or (not max-dur) (<= (trace-record-duration tr) max-dur))
                   (or (not t0) (>= (trace-record-start-time tr) t0))
                   (or (not t1) (<= (trace-record-end-time tr) t1))))
            filtered-traces))
  
  ;; Sort descending by start-time
  (take (sort final-list > #:key trace-record-start-time)
        (min limit (length final-list))))

;; Lists all distinct services
(define (storage-list-services engine)
  (storage-flush! engine)
  (sort (hash-keys (inverted-index-service-postings (unbox (storage-engine-global-index engine))))
        string<?))

;; Storage telemetry stats
(define (storage-stats engine)
  (hash 'buffered_spans (ring-buffer-size (storage-engine-ring-buf engine))
        'total_spans_indexed (unbox (storage-engine-span-counter engine))
        'total_traces (hash-count (unbox (storage-engine-trace-map engine)))
        'distinct_services (length (storage-list-services engine))))

(define (storage-close! engine)
  (storage-flush! engine)
  (when (storage-engine-wal-w engine)
    (wal-close! (storage-engine-wal-w engine))))
