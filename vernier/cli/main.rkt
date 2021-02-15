#lang racket/base

;;; vernier/cli/main.rkt
;;; Unified Command Line Interface for Vernier.
;;; Provides subcommands:
;;;   server    - Run the Vernier distributed tracing HTTP daemon and console
;;;   trace     - Inspect and render ASCII waterfall timeline for a trace ID
;;;   query     - Execute VQL query against a WAL or active traces
;;;   benchmark - Run high-throughput in-memory ingestion and DAG analysis benchmark
;;;   demo      - Launch synthetic microservices cluster simulation

(require racket/cmdline
         racket/string
         racket/list
         racket/format
         racket/file
         "../core/time.rkt"
         "../core/id.rkt"
         "../core/span.rkt"
         "../core/trace.rkt"
         "../dag/critical_path.rkt"
         "../storage/engine.rkt"
         "../query/executor.rkt"
         "../profiler/waterfall.rkt"
         "../server/server.rkt")

(provide run-cli)

(define (print-banner)
  (displayln "================================================================================")
  (displayln "  VERNIER : Distributed Tracing, Critical Path Analysis & Telemetry Engine")
  (displayln "================================================================================"))

;; Benchmark runner
(define (run-benchmark span-count)
  (printf "Benchmarking Vernier engine with ~a spans...\n" span-count)
  (define engine (make-storage-engine))
  (define start-ns (current-monotonic-nanos))
  
  (define num-traces (max 1 (quotient span-count 10)))
  (for ([t (in-range num-traces)])
    (define tid (make-trace-id))
    (define root-id (make-span-id))
    (define t-start (* t 1000000))
    ;; Root span
    (storage-write-span! engine
                         (make-span #:trace-id tid
                                    #:span-id root-id
                                    #:name "edge-request"
                                    #:service-name "gateway"
                                    #:start-time t-start
                                    #:end-time (+ t-start 5000000)
                                    #:status-code 'ok))
    ;; 9 child spans
    (for ([c (in-range 9)])
      (storage-write-span! engine
                           (make-span #:trace-id tid
                                      #:span-id (make-span-id)
                                      #:parent-id root-id
                                      #:name (format "child-op-~a" c)
                                      #:service-name (if (= (modulo c 2) 0) "billing" "inventory")
                                      #:start-time (+ t-start (* (+ c 1) 200000))
                                      #:end-time (+ t-start (* (+ c 2) 200000))
                                      #:status-code 'ok))))
  
  (storage-flush! engine)
  (define ingest-elapsed-ns (- (current-monotonic-nanos) start-ns))
  (define ingest-elapsed-sec (/ ingest-elapsed-ns 1000000000.0))
  (define throughput (round (/ span-count (max 0.000001 ingest-elapsed-sec))))
  
  (printf "Ingested ~a spans in ~a s (~a spans/sec)\n"
          span-count
          (~r ingest-elapsed-sec #:precision '(= 3))
          (~r throughput #:precision '(= 0)))
  
  ;; Query benchmark
  (define q-start-ns (current-monotonic-nanos))
  (define traces (storage-search engine #:service "gateway" #:limit 1000))
  (define q-elapsed-ns (- (current-monotonic-nanos) q-start-ns))
  (printf "Queried ~a matching traces in ~a ms\n"
          (length traces)
          (~r (/ q-elapsed-ns 1000000.0) #:precision '(= 3)))
  
  ;; Critical path analysis benchmark
  (define cp-start-ns (current-monotonic-nanos))
  (define cp-count 0)
  (for ([tr (in-list traces)])
    (analyze-critical-path tr)
    (set! cp-count (+ cp-count 1)))
  (define cp-elapsed-ns (- (current-monotonic-nanos) cp-start-ns))
  (printf "Calculated critical path for ~a traces in ~a ms (~a us/trace)\n"
          cp-count
          (~r (/ cp-elapsed-ns 1000000.0) #:precision '(= 3))
          (if (> cp-count 0)
              (~r (/ (/ cp-elapsed-ns 1000.0) cp-count) #:precision '(= 1))
              0))
  
  (storage-close! engine)
  (displayln "Benchmark completed successfully."))

;; Inspect trace by ID and print ASCII waterfall
(define (run-trace-view tid wal-dir)
  (define engine (make-storage-engine #:wal-dir wal-dir))
  (define tr (storage-get-trace engine tid))
  (if (not tr)
      (begin
        (eprintf "Error: Trace ID '~a' not found.\n" tid)
        (storage-close! engine)
        1)
      (let ([cp (analyze-critical-path tr)])
        (printf "Trace: ~a\n" (trace-record-trace-id tr))
        (printf "Duration: ~a\n" (format-duration-human (trace-record-duration tr)))
        (printf "Spans: ~a | Services: ~a\n\n"
                (length (trace-record-spans tr))
                (string-join (trace-record-services tr) ", "))
        (displayln (render-waterfall-ascii tr #:width 80))
        (newline)
        (printf "Critical Path Duration: ~a (~a% of total)\n"
                (format-duration-human (critical-path-result-critical-duration cp))
                (if (> (trace-record-duration tr) 0)
                    (~r (* 100.0 (/ (critical-path-result-critical-duration cp) (trace-record-duration tr)))
                        #:precision '(= 1))
                    "100.0"))
        (displayln "Service Critical Attributions:")
        (for ([attr (in-list (critical-path-result-attributions cp))])
          (printf "  - ~a: ~a (~a%)\n"
                  (service-attribution-service-name attr)
                  (format-duration-human (service-attribution-critical-duration attr))
                  (~r (service-attribution-percentage attr) #:precision '(= 1))))
        (storage-close! engine)
        0)))

;; Execute VQL query
(define (run-query vql-str wal-dir)
  (define engine (make-storage-engine #:wal-dir wal-dir))
  (define traces (storage-search engine #:limit 1000))
  (define res (execute-vql vql-str traces))
  (define metrics (query-result-metrics res))
  (printf "Query: ~a\n" vql-str)
  (printf "Matches: ~a | Errors: ~a | p50: ~a | p90: ~a | p99: ~a\n\n"
          (query-metrics-total-matches metrics)
          (query-metrics-error-count metrics)
          (if (query-metrics-p50-nanos metrics) (format-duration-human (query-metrics-p50-nanos metrics)) "N/A")
          (if (query-metrics-p90-nanos metrics) (format-duration-human (query-metrics-p90-nanos metrics)) "N/A")
          (if (query-metrics-p99-nanos metrics) (format-duration-human (query-metrics-p99-nanos metrics)) "N/A"))
  (for ([tr (in-list (query-result-traces res))])
    (define r (trace-record-root-span tr))
    (printf "  ~a | ~a | ~a | ~a\n"
            (trace-record-trace-id tr)
            (if r (span-service-name r) "unknown")
            (if r (span-name r) "unknown")
            (format-duration-human (trace-record-duration tr))))
  (storage-close! engine))

;; Server command
(define (run-server-cmd port host wal-dir)
  (print-banner)
  (printf "Starting Vernier Observability Engine on ~a:~a...\n" host port)
  (when wal-dir
    (printf "Write-Ahead Log persistence directory: ~a\n" wal-dir))
  (define inst (start-vernier-server #:port port #:host host #:wal-dir wal-dir))
  (printf "Web Console: http://~a:~a/\n" host port)
  (printf "OTLP / JSON Ingest Endpoint: http://~a:~a/api/v1/spans\n" host port)
  (printf "Server-Sent Events Stream: http://~a:~a/api/v1/stream\n" host port)
  (displayln "Press Ctrl+C to stop.")
  (sync (never-evt)))

;; Main CLI dispatcher
(define (run-cli args)
  (if (null? args)
      (begin
        (print-banner)
        (displayln "Usage: vernier <command> [options]")
        (displayln "")
        (displayln "Commands:")
        (displayln "  server      Run HTTP daemon, REST API and Web Console")
        (displayln "  trace       Inspect and visualize a trace by ID")
        (displayln "  query       Execute VQL query")
        (displayln "  benchmark   Run in-memory ingestion and analysis benchmark")
        (displayln "  demo        Run synthetic microservice cluster simulator")
        0)
      (let ([subcommand (car args)]
            [sub-args (cdr args)])
        (case (string->symbol subcommand)
          [(server)
           (define port 9876)
           (define host "127.0.0.1")
           (define wal-dir #f)
           (command-line
            #:program "vernier server"
            #:argv sub-args
            #:once-each
            [("-p" "--port") p "Port number to listen on (default: 9876)"
             (set! port (string->number p))]
            [("-H" "--host") h "Host address to bind to (default: 127.0.0.1)"
             (set! host h)]
            [("-w" "--wal-dir") w "WAL directory for crash recovery and persistence"
             (set! wal-dir w)])
           (run-server-cmd port host wal-dir)]
          
          [(benchmark)
           (define count 5000)
           (command-line
            #:program "vernier benchmark"
            #:argv sub-args
            #:once-each
            [("-n" "--count") n "Number of spans to generate and ingest (default: 5000)"
             (set! count (string->number n))])
           (run-benchmark count)]
          
          [(trace)
           (define wal-dir #f)
           (define tid #f)
           (command-line
            #:program "vernier trace"
            #:argv sub-args
            #:once-each
            [("-w" "--wal-dir") w "WAL directory"
             (set! wal-dir w)]
            #:args (trace-id)
            (set! tid trace-id))
           (if tid
               (run-trace-view tid wal-dir)
               (eprintf "Error: trace-id required\n"))]
          
          [(query)
           (define wal-dir #f)
           (define vql-str #f)
           (command-line
            #:program "vernier query"
            #:argv sub-args
            #:once-each
            [("-w" "--wal-dir") w "WAL directory"
             (set! wal-dir w)]
            #:args (query-string)
            (set! vql-str query-string))
           (if vql-str
               (run-query vql-str wal-dir)
               (eprintf "Error: query-string required\n"))]
          
          [(demo)
           (displayln "Loading synthetic microservice cluster simulator...")
           ;; Dynamic require to avoid cyclic loading
           (define run-demo (dynamic-require "../simulator/mesh.rkt" 'run-simulation-demo))
           (run-demo sub-args)]
          
          [else
           (eprintf "Unknown command: ~a\nRun 'vernier' for usage.\n" subcommand)
           (exit 1)]))))
