#lang racket/base

;;; tests/unit/test_server.rkt
;;; Unit test suite for HTTP router, API handlers, and TCP server lifecycle.

(require rackunit
         racket/tcp
         racket/string
         json
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/server/router.rkt"
         "../../vernier/server/handler.rkt"
         "../../vernier/server/server.rkt"
         "../../vernier/ui/console.rkt")

(test-case "Query string parser"
  (define q (parse-query-string "service=web&limit=25&debug"))
  (check-equal? (hash-ref q "service") "web")
  (check-equal? (hash-ref q "limit") "25")
  (check-equal? (hash-ref q "debug") #t))

(test-case "Web Console HTML rendering"
  (define html (render-console-html))
  (check-pred string? html)
  (check-true (string-contains? html "<!DOCTYPE html>"))
  (check-true (string-contains? html "VERNIER")))

(test-case "Server startup, span ingest, search, and shutdown"
  ;; Discover open OS ephemeral port
  (define probe-listener (tcp-listen 0 1 #t "127.0.0.1"))
  (define-values (_h test-port _rh _rp) (tcp-addresses probe-listener #t))
  (tcp-close probe-listener)
  
  (define inst (start-vernier-server #:port test-port #:host "127.0.0.1"))
  (sleep 0.15)
  
  (define tid (make-trace-id))
  (define sid (make-span-id))
  (define s (make-span #:trace-id tid #:span-id sid #:name "HTTP GET /health"
                       #:service-name "healthcheck" #:start-time 1000 #:end-time 2000
                       #:status-code 'ok))
  
  ;; 1. Send POST /api/v1/spans
  (define-values (in1 out1) (tcp-connect "127.0.0.1" test-port))
  (define payload-bytes (string->bytes/utf-8 (jsexpr->string (list (span->hash s)))))
  (fprintf out1 "POST /api/v1/spans HTTP/1.1\r\n")
  (fprintf out1 "Host: 127.0.0.1\r\n")
  (fprintf out1 "Content-Type: application/json\r\n")
  (fprintf out1 "Content-Length: ~a\r\n\r\n" (bytes-length payload-bytes))
  (write-bytes payload-bytes out1)
  (flush-output out1)
  
  (define resp-line1 (read-line in1 'any))
  (check-true (string-contains? resp-line1 "202 Accepted"))
  (close-input-port in1)
  (close-output-port out1)
  
  ;; 2. Send GET /api/v1/traces
  (define-values (in2 out2) (tcp-connect "127.0.0.1" test-port))
  (fprintf out2 "GET /api/v1/traces HTTP/1.1\r\n")
  (fprintf out2 "Host: 127.0.0.1\r\n\r\n")
  (flush-output out2)
  
  (define resp-line2 (read-line in2 'any))
  (check-true (string-contains? resp-line2 "200 OK"))
  (close-input-port in2)
  (close-output-port out2)
  
  ;; 3. Send GET / (Console HTML)
  (define-values (in3 out3) (tcp-connect "127.0.0.1" test-port))
  (fprintf out3 "GET / HTTP/1.1\r\n")
  (fprintf out3 "Host: 127.0.0.1\r\n\r\n")
  (flush-output out3)
  
  (define resp-line3 (read-line in3 'any))
  (check-true (string-contains? resp-line3 "200 OK"))
  (close-input-port in3)
  (close-output-port out3)

  ;; 4. Send GET /api/v1/traces/<trace-id> (Single trace inspection)
  (define-values (in4 out4) (tcp-connect "127.0.0.1" test-port))
  (fprintf out4 "GET /api/v1/traces/~a HTTP/1.1\r\n" tid)
  (fprintf out4 "Host: 127.0.0.1\r\n\r\n")
  (flush-output out4)
  (define resp-line4 (read-line in4 'any))
  (check-true (string-contains? resp-line4 "200 OK"))
  (close-input-port in4)
  (close-output-port out4)

  ;; 5. Send GET /api/v1/traces/<trace-id>/flamegraph
  (define-values (in5 out5) (tcp-connect "127.0.0.1" test-port))
  (fprintf out5 "GET /api/v1/traces/~a/flamegraph HTTP/1.1\r\n" tid)
  (fprintf out5 "Host: 127.0.0.1\r\n\r\n")
  (flush-output out5)
  (define resp-line5 (read-line in5 'any))
  (check-true (string-contains? resp-line5 "200 OK"))
  (close-input-port in5)
  (close-output-port out5)

  ;; 6. Send GET /api/v1/topology
  (define-values (in6 out6) (tcp-connect "127.0.0.1" test-port))
  (fprintf out6 "GET /api/v1/topology HTTP/1.1\r\n")
  (fprintf out6 "Host: 127.0.0.1\r\n\r\n")
  (flush-output out6)
  (define resp-line6 (read-line in6 'any))
  (check-true (string-contains? resp-line6 "200 OK"))
  (close-input-port in6)
  (close-output-port out6)
  
  ;; Clean shutdown
  (stop-vernier-server! inst))
