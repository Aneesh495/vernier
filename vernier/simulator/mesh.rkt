#lang racket/base

;;; vernier/simulator/mesh.rkt
;;; Microservice cluster simulator and synthetic workload generator.
;;; Simulates realistic distributed request graphs with injected latency anomalies,
;;; N+1 query regressions, retry storms, and fanout explosions.

(require racket/contract
         racket/cmdline
         racket/random
         racket/string
         racket/list
         racket/format
         racket/tcp
         json
         "../core/time.rkt"
         "../core/id.rkt"
         "../core/span.rkt"
         "../core/trace.rkt"
         "../storage/engine.rkt"
         "../server/server.rkt"
         "../profiler/waterfall.rkt")

(provide
 (contract-out
  ;; Scenario generators
  [generate-normal-checkout-trace (->* () (#:base-time exact-integer?) (listof span?))]
  [generate-n-plus-one-trace (->* () (#:base-time exact-integer? #:item-count exact-positive-integer?) (listof span?))]
  [generate-retry-storm-trace (->* () (#:base-time exact-integer?) (listof span?))]
  [generate-fanout-explosion-trace (->* () (#:base-time exact-integer? #:fanout exact-positive-integer?) (listof span?))]
  
  ;; Batch traffic generator
  [simulate-traffic (->* (exact-positive-integer?)
                         (#:error-rate real?
                          #:anomaly-rate real?)
                         (listof (listof span?)))]
  
  ;; Demo execution
  [run-simulation-demo (-> (listof string?) void?)]))

;; Helper to make span with relative timing offset
(define (sim-span #:trace-id tid
                  #:span-id sid
                  #:parent-id [pid #f]
                  #:name name
                  #:service svc
                  #:kind [kind 'internal]
                  #:start-offset-ms start-ms
                  #:duration-ms dur-ms
                  #:base-time base-t
                  #:status [stat 'ok]
                  #:status-message [stat-msg ""]
                  #:attributes [attrs (hash)])
  (define s-ns (+ base-t (* start-ms 1000000)))
  (define e-ns (+ s-ns (* dur-ms 1000000)))
  (make-span #:trace-id tid
             #:span-id sid
             #:parent-id pid
             #:name name
             #:service-name svc
             #:kind kind
             #:start-time s-ns
             #:end-time e-ns
             #:status-code stat
             #:status-message stat-msg
             #:attributes attrs))

;; 1. Normal E-Commerce Checkout Flow (multi-tier, concurrent branches)
(define (generate-normal-checkout-trace #:base-time [base-t (current-time-nanos)])
  (define tid (make-trace-id))
  (define gateway-id (make-span-id))
  (define auth-id (make-span-id))
  (define order-id (make-span-id))
  (define inventory-id (make-span-id))
  (define payment-id (make-span-id))
  (define fraud-id (make-span-id))
  (define stripe-id (make-span-id))
  (define notify-id (make-span-id))
  (define db-id (make-span-id))

  (list
   ;; 1. API Gateway Entrypoint
   (sim-span #:trace-id tid #:span-id gateway-id #:name "POST /v2/checkout"
             #:service "api-gateway" #:kind 'server
             #:start-offset-ms 0 #:duration-ms 185 #:base-time base-t
             #:attributes (hash 'http.method "POST" 'http.route "/v2/checkout" 'client.ip "198.51.100.14"))
   
   ;; 2. Auth Service JWT validation
   (sim-span #:trace-id tid #:span-id auth-id #:parent-id gateway-id
             #:name "validate_jwt" #:service "auth-service" #:kind 'internal
             #:start-offset-ms 5 #:duration-ms 22 #:base-time base-t
             #:attributes (hash 'auth.scheme "Bearer" 'user.id "usr_99812"))
   
   ;; 3. Order Service coordinator
   (sim-span #:trace-id tid #:span-id order-id #:parent-id gateway-id
             #:name "create_order_saga" #:service "order-service" #:kind 'internal
             #:start-offset-ms 32 #:duration-ms 148 #:base-time base-t
             #:attributes (hash 'order.items_count 3 'order.currency "USD"))
   
   ;; 4. Check Inventory (concurrent branch A)
   (sim-span #:trace-id tid #:span-id inventory-id #:parent-id order-id
             #:name "reserve_stock" #:service "catalog-service" #:kind 'client
             #:start-offset-ms 38 #:duration-ms 35 #:base-time base-t
             #:attributes (hash 'warehouse.id "wh-east-2" 'sku.count 3))
   
   ;; 5. Payment Service (concurrent branch B)
   (sim-span #:trace-id tid #:span-id payment-id #:parent-id order-id
             #:name "process_payment" #:service "payment-service" #:kind 'client
             #:start-offset-ms 76 #:duration-ms 82 #:base-time base-t
             #:attributes (hash 'payment.amount 128.50 'payment.provider "stripe"))
   
   ;; 6. Fraud check under payment
   (sim-span #:trace-id tid #:span-id fraud-id #:parent-id payment-id
             #:name "eval_fraud_risk" #:service "payment-service" #:kind 'internal
             #:start-offset-ms 80 #:duration-ms 18 #:base-time base-t
             #:attributes (hash 'fraud.score 0.02 'fraud.decision "allow"))
   
   ;; 7. External Stripe HTTP call
   (sim-span #:trace-id tid #:span-id stripe-id #:parent-id payment-id
             #:name "POST https://api.stripe.com/v1/charges" #:service "payment-service" #:kind 'client
             #:start-offset-ms 102 #:duration-ms 52 #:base-time base-t
             #:attributes (hash 'peer.service "stripe-api" 'http.status_code 200))
   
   ;; 8. Order State Persistence
   (sim-span #:trace-id tid #:span-id db-id #:parent-id order-id
             #:name "UPDATE orders SET status = 'CONFIRMED'" #:service "order-service" #:kind 'client
             #:start-offset-ms 160 #:duration-ms 15 #:base-time base-t
             #:attributes (hash 'db.system "postgresql" 'db.statement "UPDATE orders..."))
   
   ;; 9. Asynchronous Notification dispatch
   (sim-span #:trace-id tid #:span-id notify-id #:parent-id order-id
             #:name "dispatch_confirmation_email" #:service "notification-service" #:kind 'producer
             #:start-offset-ms 168 #:duration-ms 8 #:base-time base-t
             #:attributes (hash 'messaging.destination "email.queue"))))

;; 2. N+1 Query Regression Scenario
(define (generate-n-plus-one-trace #:base-time [base-t (current-time-nanos)]
                                   #:item-count [n 25])
  (define tid (make-trace-id))
  (define gateway-id (make-span-id))
  (define catalog-id (make-span-id))
  (define spans (list))

  ;; Root
  (set! spans
        (cons (sim-span #:trace-id tid #:span-id gateway-id #:name "GET /api/cart"
                        #:service "api-gateway" #:kind 'server
                        #:start-offset-ms 0 #:duration-ms (+ 40 (* n 12)) #:base-time base-t
                        #:attributes (hash 'http.route "/api/cart"))
              spans))
  
  ;; Catalog loader
  (set! spans
        (cons (sim-span #:trace-id tid #:span-id catalog-id #:parent-id gateway-id
                        #:name "load_cart_items" #:service "catalog-service" #:kind 'internal
                        #:start-offset-ms 15 #:duration-ms (+ 15 (* n 12)) #:base-time base-t
                        #:attributes (hash 'cart.item_count n))
              spans))
  
  ;; Sequential N+1 queries executed in a loop
  (for ([i (in-range n)])
    (define offset (+ 25 (* i 12)))
    (set! spans
          (cons (sim-span #:trace-id tid #:span-id (make-span-id) #:parent-id catalog-id
                          #:name "SELECT * FROM product_details WHERE id = ?"
                          #:service "catalog-service" #:kind 'client
                          #:start-offset-ms offset #:duration-ms 10 #:base-time base-t
                          #:attributes (hash 'db.system "mysql" 'db.statement "SELECT * FROM product_details WHERE id = ?" 'item.index i))
                spans)))
  
  (reverse spans))

;; 3. Retry Storm Scenario (Cascading Downstream Failure)
(define (generate-retry-storm-trace #:base-time [base-t (current-time-nanos)])
  (define tid (make-trace-id))
  (define root-id (make-span-id))
  (define pay-id (make-span-id))
  (define spans (list))

  (set! spans
        (cons (sim-span #:trace-id tid #:span-id root-id #:name "POST /checkout"
                        #:service "api-gateway" #:kind 'server
                        #:start-offset-ms 0 #:duration-ms 950 #:base-time base-t
                        #:status 'error #:status-message "Payment downstream timeout"
                        #:attributes (hash 'http.status_code 504))
              spans))

  (set! spans
        (cons (sim-span #:trace-id tid #:span-id pay-id #:parent-id root-id
                        #:name "process_transaction" #:service "payment-service" #:kind 'internal
                        #:start-offset-ms 20 #:duration-ms 920 #:base-time base-t
                        #:status 'error #:status-message "Exceeded 3 retry attempts")
              spans))

  ;; 3 consecutive failed retries with increasing backoff
  (define offsets '(40 320 620))
  (for ([attempt (in-range 3)]
        [off (in-list offsets)])
    (define attempt-id (make-span-id))
    (set! spans
          (cons (sim-span #:trace-id tid #:span-id attempt-id #:parent-id pay-id
                          #:name (format "gateway_call_attempt_~a" (+ attempt 1))
                          #:service "payment-service" #:kind 'client
                          #:start-offset-ms off #:duration-ms 250 #:base-time base-t
                          #:status 'error #:status-message "HTTP 503 Service Unavailable"
                          #:attributes (hash 'retry.count (+ attempt 1)
                                             'http.status_code 503
                                             'error.reason "connection_reset"))
                spans)))
  
  (reverse spans))

;; 4. Fanout Explosion Scenario
(define (generate-fanout-explosion-trace #:base-time [base-t (current-time-nanos)]
                                         #:fanout [count 40])
  (define tid (make-trace-id))
  (define root-id (make-span-id))
  (define batch-id (make-span-id))
  (define spans (list))

  (set! spans
        (cons (sim-span #:trace-id tid #:span-id root-id #:name "POST /broadcast"
                        #:service "api-gateway" #:kind 'server
                        #:start-offset-ms 0 #:duration-ms 350 #:base-time base-t)
              spans))

  (set! spans
        (cons (sim-span #:trace-id tid #:span-id batch-id #:parent-id root-id
                        #:name "fanout_notifications" #:service "notification-service" #:kind 'internal
                        #:start-offset-ms 20 #:duration-ms 320 #:base-time base-t
                        #:attributes (hash 'batch.size count))
              spans))

  ;; Parallel worker spans
  (for ([i (in-range count)])
    (define offset (+ 30 (random 20)))
    (define dur (+ 50 (random 200)))
    (set! spans
          (cons (sim-span #:trace-id tid #:span-id (make-span-id) #:parent-id batch-id
                          #:name (format "worker_send_push_~a" i)
                          #:service "notification-service" #:kind 'producer
                          #:start-offset-ms offset #:duration-ms dur #:base-time base-t
                          #:attributes (hash 'worker.id i 'channel "fcm"))
                spans)))

  (reverse spans))

;; Generates a batch of mixed traffic
(define (simulate-traffic count
                          #:error-rate [err-rate 0.08]
                          #:anomaly-rate [anom-rate 0.15])
  (for/list ([i (in-range count)])
    (define base-t (+ (current-time-nanos) (* i 50000000))) ;; 50ms stagger
    (define roll (/ (random 1000) 1000.0))
    (cond
      [(< roll (* anom-rate 0.4))
       (generate-n-plus-one-trace #:base-time base-t #:item-count (+ 15 (random 20)))]
      [(< roll (* anom-rate 0.8))
       (generate-retry-storm-trace #:base-time base-t)]
      [(< roll anom-rate)
       (generate-fanout-explosion-trace #:base-time base-t #:fanout (+ 25 (random 30)))]
      [else
       (generate-normal-checkout-trace #:base-time base-t)])))

;; Demo CLI Command
(define (run-simulation-demo args)
  (define port 9876)
  (define count 20)
  (define delay-ms 200)
  
  (command-line
   #:program "vernier demo"
   #:argv args
   #:once-each
   [("-p" "--port") p "Port of running Vernier server (default: 9876)"
    (set! port (string->number p))]
   [("-n" "--count") c "Number of traces to generate and transmit (default: 20)"
    (set! count (string->number c))]
   [("-d" "--delay") d "Delay between requests in ms (default: 200)"
    (set! delay-ms (string->number d))])
  
  (printf "Generating ~a synthetic distributed transactions to http://127.0.0.1:~a/api/v1/spans...\n"
          count port)
  
  (define traffic (simulate-traffic count))
  (define sent-count 0)
  
  (for ([trace-spans (in-list traffic)]
        [idx (in-naturals 1)])
    (with-handlers ([exn:fail? (lambda (e)
                                 (eprintf "Failed to send trace ~a: ~a\n" idx (exn-message e)))])
      (define-values (in out) (tcp-connect "127.0.0.1" port))
      (define payload (jsexpr->string (map span->hash trace-spans)))
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
      (set! sent-count (+ sent-count 1))
      (printf "  [~a/~a] Ingested ~a spans (~a)\n"
              idx count (length trace-spans) (span-name (car trace-spans)))
      (sleep (/ delay-ms 1000.0))))
  
  (printf "Simulation finished! Successfully streamed ~a traces to Vernier server.\n" sent-count)
  (printf "View live telemetry and flamegraphs in browser at http://127.0.0.1:~a/\n" port))
