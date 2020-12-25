#lang racket/base

;;; vernier/storage/partition.rkt
;;; Time-partitioned storage segments with automatic age-based retention.
;;; Organizes historical spans into queryable temporal chunks.

(require racket/contract
         racket/list
         racket/set
         "../core/span.rkt"
         "../core/trace.rkt"
         "column.rkt"
         "index.rkt")

(provide
 (contract-out
  ;; Partition record
  [struct time-partition ([partition-id exact-integer?]
                          [start-nanos exact-integer?]
                          [end-nanos exact-integer?]
                          [spans (listof span?)]
                          [traces hash?]          ; tid -> trace-record
                          [column-block (or/c column-block? #f)]
                          [index inverted-index?])]
  
  [make-time-partition (-> exact-integer? exact-integer? exact-integer? time-partition?)]
  [partition-add-span (-> time-partition? span? time-partition?)]
  [partition-seal (-> time-partition? time-partition?)]
  
  ;; Partition manager
  [struct partition-manager ([segment-duration-nanos exact-integer?]
                             [retention-nanos exact-integer?]
                             [partitions hash?])]
  
  [make-partition-manager (->* () (exact-integer? exact-integer?) partition-manager?)]
  [partition-manager-insert (-> partition-manager? span? partition-manager?)]
  [partition-manager-purge-expired (-> partition-manager? exact-integer? partition-manager?)]
  [partition-manager-active-traces (-> partition-manager? (listof trace-record?))]))

(struct time-partition (partition-id
                        start-nanos
                        end-nanos
                        spans
                        traces
                        column-block
                        index)
  #:transparent)

;; Constructs empty time partition
(define (make-time-partition pid start end)
  (time-partition pid start end '() (hash) #f (make-inverted-index)))

;; Appends span to active partition
(define (partition-add-span part s)
  (define new-spans (cons s (time-partition-spans part)))
  (define new-idx (index-add-span (time-partition-index part) s))
  
  ;; Update traces map
  (define tid (span-trace-id s))
  (define current-traces (time-partition-traces part))
  (define updated-traces
    (let ([existing-tr (hash-ref current-traces tid #f)])
      (if existing-tr
          (hash-set current-traces tid (trace-add-span existing-tr s))
          (hash-set current-traces tid (make-trace (list s))))))
  
  (struct-copy time-partition part
               [spans new-spans]
               [traces updated-traces]
               [index new-idx]))

;; Seals partition: builds columnar block for vectorized scans
(define (partition-seal part)
  (define cb (make-column-block (time-partition-spans part)))
  (struct-copy time-partition part [column-block cb]))

;; Partition manager orchestrating partitions across time
(struct partition-manager (segment-duration-nanos retention-nanos partitions)
  #:transparent)

;; Default: 1-hour segments (3.6e12 nanos), 24-hour retention (8.64e13 nanos)
(define (make-partition-manager [segment-dur 3600000000000]
                                [retention 86400000000000])
  (partition-manager segment-dur retention (hash)))

;; Inserts span into appropriate temporal partition
(define (partition-manager-insert pm s)
  (define seg-dur (partition-manager-segment-duration-nanos pm))
  (define pid (quotient (span-start-time s) seg-dur))
  (define start (* pid seg-dur))
  (define end (+ start seg-dur))
  
  (define current-parts (partition-manager-partitions pm))
  (define part
    (hash-ref current-parts pid
              (lambda () (make-time-partition pid start end))))
  (define updated-part (partition-add-span part s))
  
  (struct-copy partition-manager pm
               [partitions (hash-set current-parts pid updated-part)]))

;; Purges partitions whose end-time is older than (now - retention)
(define (partition-manager-purge-expired pm now-nanos)
  (define cutoff (- now-nanos (partition-manager-retention-nanos pm)))
  (define active-parts
    (for/hash ([(pid part) (in-hash (partition-manager-partitions pm))]
               #:when (>= (time-partition-end-nanos part) cutoff))
      (values pid part)))
  (struct-copy partition-manager pm [partitions active-parts]))

;; Returns all unique trace records across active partitions
(define (partition-manager-active-traces pm)
  (define all-traces
    (for/fold ([acc (hash)])
              ([(pid part) (in-hash (partition-manager-partitions pm))])
      (for/fold ([inner-acc acc])
                ([(tid tr) (in-hash (time-partition-traces part))])
        (hash-set inner-acc tid tr))))
  (hash-values all-traces))
