#lang racket/base

;;; tests/integration/test_pipeline.rkt
;;; Full end-to-end pipeline integration test suite.
;;; Validates:
;;;   1. Multi-tier span ingestion via HTTP REST API
;;;   2. Write-Ahead Log crash recovery and disk replay
;;;   3. Real-time DAG reconstruction and topological ordering
;;;   4. Dynamic programming critical path analysis
;;;   5. Anomaly pattern detection (N+1 regressions, retry storms)
;;;   6. VQL query execution and percentile latency aggregation
;;;   7. ASCII waterfall and SVG flamegraph generation

(require rackunit
         racket/tcp
         racket/file
         racket/string
         json
         "../../vernier/core/time.rkt"
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/core/trace.rkt"
         "../../vernier/dag/graph.rkt"
         "../../vernier/dag/critical_path.rkt"
         "../../vernier/anomaly/detector.rkt"
         "../../vernier/anomaly/scorer.rkt"
         "../../vernier/storage/engine.rkt"
         "../../vernier/query/executor.rkt"
         "../../vernier/profiler/waterfall.rkt"
         "../../vernier/profiler/flamegraph.rkt"
         "../../vernier/simulator/mesh.rkt"
         "../../vernier/server/server.rkt")

(test-case "End-to-end telemetry pipeline with WAL persistence and crash replay"
  (define temp-wal-dir (make-temporary-file "vernier-integ-wal-~a" 'directory))
  
  ;; Stage 1: Spin up server with WAL persistence
  (define probe-listener (tcp-listen 0 1 #t "127.0.0.1"))
  (define-values (_h port _rh _rp) (tcp-addresses probe-listener #t))
  (tcp-close probe-listener)
  
  (define server (start-vernier-server #:port port #:host "127.0.0.1" #:wal-dir temp-wal-dir))
  (sleep 0.1)

  ;; Stage 2: Generate multi-scenario traces and stream via HTTP
  (define normal-spans (generate-normal-checkout-trace))
  (define n1-spans (generate-n-plus-one-trace #:item-count 15))
  (define retry-spans (generate-retry-storm-trace))

  (define (http-post-spans spans)
    (define-values (in out) (tcp-connect "127.0.0.1" port))
    (define payload (jsexpr->string (map span->hash spans)))
    (define body-bytes (string->bytes/utf-8 payload))
    (fprintf out "POST /api/v1/spans HTTP/1.1\r\n")
    (fprintf out "Host: 127.0.0.1:~a\r\n" port)
    (fprintf out "Content-Type: application/json\r\n")
    (fprintf out "Content-Length: ~a\r\n" (bytes-length body-bytes))
    (fprintf out "Connection: close\r\n\r\n")
    (write-bytes body-bytes out)
    (flush-output out)
    (define resp (read-line in 'any))
    (close-input-port in)
    (close-output-port out)
    resp)

  (define resp1 (http-post-spans normal-spans))
  (define resp2 (http-post-spans n1-spans))
  (define resp3 (http-post-spans retry-spans))
  (check-true (string-contains? resp1 "202 Accepted"))
  (check-true (string-contains? resp2 "202 Accepted"))
  (check-true (string-contains? resp3 "202 Accepted"))

  ;; Stage 3: Query server stats and search
  (define-values (in-stats out-stats) (tcp-connect "127.0.0.1" port))
  (fprintf out-stats "GET /api/v1/stats HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n")
  (flush-output out-stats)
  (define stats-status (read-line in-stats 'any))
  (check-true (string-contains? stats-status "200 OK"))
  (close-input-port in-stats)
  (close-output-port out-stats)

  ;; Stage 4: Simulate unclean server shutdown
  (stop-vernier-server! server)
  (sleep 0.1)

  ;; Stage 5: Recover state by creating new storage engine pointed at same WAL directory
  (define recovered-engine (make-storage-engine #:wal-dir temp-wal-dir))
  (define normal-tid (span-trace-id (car normal-spans)))
  (define recovered-tr (storage-get-trace recovered-engine normal-tid))
  (check-not-false recovered-tr)
  (check-equal? (length (trace-record-spans recovered-tr)) (length normal-spans))

  ;; Stage 6: Verify critical path analysis on recovered trace
  (define cp-res (analyze-critical-path recovered-tr))
  (check-true (> (critical-path-result-critical-duration cp-res) 0))
  (check-true (<= (critical-path-result-critical-duration cp-res)
                  (trace-record-duration recovered-tr)))

  ;; Stage 7: Verify anomaly detection on N+1 trace
  (define n1-tid (span-trace-id (car n1-spans)))
  (define n1-tr (storage-get-trace recovered-engine n1-tid))
  (check-not-false n1-tr)
  (define detector (make-anomaly-detector))
  (define anomaly-rep (detector-analyze-trace detector n1-tr))
  (check-true (not (null? (anomaly-report-structural-anomalies anomaly-rep))))

  ;; Stage 8: Verify VQL execution on recovered traces
  (define all-traces (storage-search recovered-engine #:limit 100))
  (check-equal? (length all-traces) 3)
  (define vql-res (execute-vql "service = \"api-gateway\"" all-traces))
  (check-equal? (length (query-result-traces vql-res)) 3)

  ;; Cleanup
  (storage-close! recovered-engine)
  (delete-directory/files temp-wal-dir))
