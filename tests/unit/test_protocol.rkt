#lang racket/base

;;; tests/unit/test_protocol.rkt
;;; Unit test suite for W3C TraceContext headers, OpenTelemetry OTLP JSON,
;;; and compact binary wire serialization.

(require rackunit
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/protocol/w3c.rkt"
         "../../vernier/protocol/otlp.rkt"
         "../../vernier/protocol/binary.rkt")

(test-case "W3C TraceContext header injection and extraction"
  (define tid (make-trace-id))
  (define sid (make-span-id))
  (define ctx (make-trace-context tid sid default-trace-flags (hash "congo" "t61rcWkgMzE")))
  
  (define headers (inject-w3c-headers ctx))
  (check-equal? (length headers) 2)
  
  (define extracted (extract-w3c-headers headers))
  (check-not-false extracted)
  (check-equal? (trace-context-trace-id extracted) tid)
  (check-equal? (trace-context-span-id extracted) sid)
  (check-equal? (hash-ref (trace-context-tracestate extracted) "congo") "t61rcWkgMzE"))

(test-case "OpenTelemetry OTLP JSON ingest and export"
  (define tid (make-trace-id))
  (define sid (make-span-id))
  (define s (make-span #:trace-id tid #:span-id sid #:name "HTTP GET"
                       #:service-name "checkout-service" #:start-time 1000 #:end-time 2000
                       #:status-code 'ok #:attributes (hasheq 'http.status 200)))
  
  (define otlp-json (spans->otlp-json (list s) "checkout-service"))
  (check-pred string? otlp-json)
  
  ;; Re-parse back into Vernier span
  (define parsed-spans (parse-otlp-json otlp-json))
  (check-equal? (length parsed-spans) 1)
  (define restored (car parsed-spans))
  (check-equal? (span-trace-id restored) tid)
  (check-equal? (span-span-id restored) sid)
  (check-equal? (span-name restored) "HTTP GET")
  (check-equal? (span-service-name restored) "checkout-service")
  (check-equal? (span-duration restored) 1000))

(test-case "Compact binary wire serialization round-trip"
  (define tid (make-trace-id))
  (define sid (make-span-id))
  (define pid (make-span-id))
  
  (define s (make-span #:trace-id tid #:span-id sid #:parent-id pid #:name "RPC Query"
                       #:service-name "inventory" #:kind 'client
                       #:start-time 5000000000 #:end-time 5150000000 #:status-code 'error))
  
  (define bs (span->binary-bytes s))
  (check-pred bytes? bs)
  
  (define restored (binary-bytes->span bs))
  (check-equal? (span-trace-id restored) tid)
  (check-equal? (span-span-id restored) sid)
  (check-equal? (span-parent-id restored) pid)
  (check-equal? (span-name restored) "RPC Query")
  (check-equal? (span-service-name restored) "inventory")
  (check-equal? (span-kind restored) 'client)
  (check-equal? (span-status-code restored) 'error)
  (check-equal? (span-duration restored) 150000000))
