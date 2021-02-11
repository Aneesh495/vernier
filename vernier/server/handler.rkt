#lang racket/base

;;; vernier/server/handler.rkt
;;; REST API route dispatchers and request handling logic.
;;; Bridges HTTP transport with storage, anomaly detection, profiler, and SSE hub.

(require racket/contract
         racket/string
         racket/list
         json
         "../core/id.rkt"
         "../core/span.rkt"
         "../core/trace.rkt"
         "../dag/critical_path.rkt"
         "../anomaly/detector.rkt"
         "../anomaly/scorer.rkt"
         "../storage/engine.rkt"
         "../query/executor.rkt"
         "../profiler/waterfall.rkt"
         "../profiler/flamegraph.rkt"
         "../profiler/topology.rkt"
         "../protocol/otlp.rkt"
         "router.rkt"
         "sse.rkt")

(provide
 (contract-out
  ;; Server state bundle
  [struct server-state ([storage storage-engine?]
                        [detector anomaly-detector?]
                        [sse sse-hub?])]
  
  [make-server-state (->* () (#:wal-dir (or/c path-string? #f)) server-state?)]
  [handle-api-request (-> server-state? http-request? output-port? (or/c http-response? #f))]))

(struct server-state (storage detector sse)
  #:transparent)

(define (make-server-state #:wal-dir [wal-dir #f])
  (server-state (make-storage-engine #:wal-dir wal-dir)
                (make-anomaly-detector)
                (make-sse-hub)))

;; Main request dispatcher
(define (handle-api-request state req out-port)
  (define method (http-request-method req))
  (define path (http-request-path req))
  (define q-params (http-request-query-params req))
  
  (cond
    ;; CORS preflight
    [(string=? method "OPTIONS")
     (make-http-response 204)]
    
    ;; Real-time Server-Sent Events stream
    [(and (string=? method "GET") (string=? path "/api/v1/stream"))
     (sse-subscribe! (server-state-sse state) out-port)
     #f] ;; SSE stream keeps connection open, response already sent
    
    ;; Ingest Spans (Native JSON or OTLP)
    [(and (string=? method "POST")
          (or (string=? path "/api/v1/spans")
              (string=? path "/v1/traces")))
     (handle-ingest-spans state req)]
    
    ;; Search Traces (VQL or query parameters)
    [(and (string=? method "GET") (string=? path "/api/v1/traces"))
     (handle-search-traces state q-params)]
    
    ;; Flamegraph View: /api/v1/traces/<tid>/flamegraph or /api/v1/flamegraph/<tid>
    [(and (string=? method "GET")
          (or (and (string-prefix? path "/api/v1/traces/") (string-suffix? path "/flamegraph"))
              (string-prefix? path "/api/v1/flamegraph/")))
     (define tid
       (if (string-prefix? path "/api/v1/flamegraph/")
           (substring path (string-length "/api/v1/flamegraph/"))
           (let* ([sub (substring path (string-length "/api/v1/traces/"))]
                  [len-sub (string-length sub)]
                  [len-sfx (string-length "/flamegraph")])
             (substring sub 0 (- len-sub len-sfx)))))
     (handle-flamegraph state tid)]

    ;; Trace Detail with Critical Path and Anomaly Analysis
    [(and (string=? method "GET") (string-prefix? path "/api/v1/traces/"))
     (define tid (substring path (string-length "/api/v1/traces/")))
     (handle-get-trace state tid)]
    
    ;; List Services
    [(and (string=? method "GET") (string=? path "/api/v1/services"))
     (define svcs (storage-list-services (server-state-storage state)))
     (response-json (hash 'services svcs))]
    
    ;; Service Dependency Topology Graph
    [(and (string=? method "GET") (string=? path "/api/v1/topology"))
     (define traces (storage-search (server-state-storage state) #:limit 500))
     (define topo (build-service-topology traces))
     (response-json (topology->hash topo))]
    
    ;; Server Stats
    [(and (string=? method "GET") (string=? path "/api/v1/stats"))
     (define stats (storage-stats (server-state-storage state)))
     (define enriched
       (hash-set stats 'sse_subscribers (sse-client-count (server-state-sse state))))
     (response-json enriched)]
    
    ;; Not Found
    [else
     (response-not-found)]))

;; Ingest spans handler
(define (handle-ingest-spans state req)
  (define body-str (bytes->string/utf-8 (http-request-body req)))
  (with-handlers ([exn:fail? (lambda (e) (response-bad-request (exn-message e)))])
    (define json-data (string->jsexpr body-str))
    (define spans
      (cond
        ;; OTLP format payload
        [(and (hash? json-data) (or (hash-has-key? json-data 'resourceSpans)
                                    (hash-has-key? json-data "resourceSpans")))
         (parse-otlp-json json-data)]
        ;; Batch of spans: [{"trace_id": "..."}, ...]
        [(list? json-data)
         (map hash->span json-data)]
        ;; Single span object
        [(hash? json-data)
         (list (hash->span json-data))]
        [else (error "Invalid payload format; expected JSON object or array")]))
    
    (for ([s (in-list spans)])
      (storage-write-span! (server-state-storage state) s))
    
    ;; Flush and notify SSE subscribers for completed traces
    (storage-flush! (server-state-storage state))
    (define unique-tids (remove-duplicates (map span-trace-id spans)))
    (for ([tid (in-list unique-tids)])
      (define tr (storage-get-trace (server-state-storage state) tid))
      (when tr
        (sse-broadcast-trace! (server-state-sse state) tr)))
    
    (response-json (hash 'status "accepted"
                         'spans_ingested (length spans))
                   #:status 202)))

;; Search traces handler
(define (handle-search-traces state q-params)
  (define vql-query (hash-ref q-params "q" #f))
  (if vql-query
      ;; Execute VQL query against active traces
      (let* ([all-traces (storage-search (server-state-storage state) #:limit 500)]
             [res (execute-vql vql-query all-traces)])
        (response-json
         (hash 'traces (map (lambda (tr) (trace-summary->hash (trace->summary tr)))
                            (query-result-traces res))
               'metrics (hash 'total_matches (query-metrics-total-matches (query-result-metrics res))
                              'error_count (query-metrics-error-count (query-result-metrics res))
                              'p50_latency_nanos (or (query-metrics-p50-nanos (query-result-metrics res)) 0)
                              'p90_latency_nanos (or (query-metrics-p90-nanos (query-result-metrics res)) 0)
                              'p99_latency_nanos (or (query-metrics-p99-nanos (query-result-metrics res)) 0)))))
      ;; Parameterized search
      (let* ([svc (hash-ref q-params "service" #f)]
             [op (hash-ref q-params "operation" #f)]
             [stat-str (hash-ref q-params "status" #f)]
             [stat (and stat-str (string->symbol (string-downcase stat-str)))]
             [limit-str (hash-ref q-params "limit" "50")]
             [limit (or (string->number limit-str) 50)]
             [traces (storage-search (server-state-storage state)
                                     #:service svc
                                     #:operation op
                                     #:status stat
                                     #:limit limit)])
        (response-json (hash 'traces (map (lambda (tr) (trace-summary->hash (trace->summary tr))) traces)
                             'count (length traces))))))

;; Get single trace handler
(define (handle-get-trace state tid)
  (define tr (storage-get-trace (server-state-storage state) tid))
  (if (not tr)
      (response-not-found)
      (let* ([cp-res (analyze-critical-path tr)]
             [anomaly-rep (detector-analyze-trace (server-state-detector state) tr)]
             [score (score-anomaly-report anomaly-rep tr)]
             [waterfall-rows (build-waterfall-layout tr)])
        (response-json
         (hash 'trace_id (trace-record-trace-id tr)
               'duration_nanos (trace-record-duration tr)
               'start_time (trace-record-start-time tr)
               'end_time (trace-record-end-time tr)
               'span_count (length (trace-record-spans tr))
               'error_count (trace-record-error-count tr)
               'services (trace-record-services tr)
               'spans (map span->hash (trace-record-spans tr))
               'critical_path
               (hash 'critical_duration_nanos (critical-path-result-critical-duration cp-res)
                     'critical_spans (map span-span-id (critical-path-result-critical-spans cp-res))
                     'attributions (map (lambda (a)
                                          (hash 'service (service-attribution-service-name a)
                                                'duration_nanos (service-attribution-critical-duration a)
                                                'percentage (service-attribution-percentage a)))
                                        (critical-path-result-attributions cp-res)))
               'anomaly_analysis
               (hash 'score (composite-score-score score)
                     'health (symbol->string (composite-score-health score))
                     'primary_issue (composite-score-primary-issue score)
                     'factors (composite-score-contributing-factors score))
               'waterfall
               (map (lambda (w)
                      (hash 'span_id (waterfall-row-span-id w)
                            'depth (waterfall-row-depth w)
                            'service (waterfall-row-service-name w)
                            'operation (waterfall-row-operation-name w)
                            'offset_pct (waterfall-row-relative-offset-pct w)
                            'width_pct (waterfall-row-relative-width-pct w)
                            'critical (waterfall-row-critical? w)
                            'error (waterfall-row-error? w)))
                    waterfall-rows))))))

;; Flamegraph handler
(define (handle-flamegraph state tid)
  (define tr (storage-get-trace (server-state-storage state) tid))
  (if (not tr)
      (response-not-found)
      (let ([tree (trace->flamegraph-tree tr)]
            [svg (render-flamegraph-svg tr)])
        (response-json
         (hash 'trace_id tid
               'tree (flame-node->hash tree)
               'svg svg)))))

(define (flame-node->hash node)
  (hash 'name (flame-node-name node)
        'service (flame-node-service-name node)
        'value (flame-node-value-nanos node)
        'children (map flame-node->hash (flame-node-children node))))
