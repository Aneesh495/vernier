#lang racket/base

;;; vernier/server/server.rkt
;;; Multi-threaded HTTP and TCP server with custodian isolation.
;;; Drives Vernier ingestion, query API, and the embedded real-time web console.

(require racket/contract
         racket/tcp
         racket/file
         racket/path
         racket/string
         "../storage/engine.rkt"
         "router.rkt"
         "handler.rkt"
         "../ui/console.rkt")

(provide
 (contract-out
  ;; Server instance handle
  [struct server-instance ([listener tcp-listener?]
                           [port exact-positive-integer?]
                           [server-thread thread?]
                           [state server-state?])]
  
  [start-vernier-server (->* () (#:port exact-positive-integer? #:host string? #:wal-dir (or/c path-string? #f)) server-instance?)]
  [stop-vernier-server! (-> server-instance? void?)]))

(struct server-instance (listener port server-thread state)
  #:transparent)

;; Processes single incoming HTTP connection in an isolated custodian sandbox
(define (handle-connection state in out)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (define req (read-http-request in))
      (when req
        (define path (http-request-path req))
        (cond
          ;; SSE stream keeps socket open
          [(string=? path "/api/v1/stream")
           (handle-api-request state req out)]
          ;; Web console UI
          [(or (string=? path "/") (string=? path "/index.html"))
           (let ([resp (make-http-response 200
                                           #:headers (hash "content-type" "text/html; charset=utf-8")
                                           #:body (render-console-html))])
             (write-http-response resp out)
             (close-output-port out)
             (close-input-port in))]
          ;; Standard REST API calls
          [else
           (let ([resp (handle-api-request state req out)])
             (when resp
               (write-http-response resp out))
             (close-output-port out)
             (close-input-port in))]))))
  (custodian-shutdown-all cust))

;; Starts Vernier background server
(define (start-vernier-server #:port [port 8080]
                              #:host [host "127.0.0.1"]
                              #:wal-dir [wal-dir #f])
  (define listener (tcp-listen port 512 #t host))
  (define state (make-server-state #:wal-dir wal-dir))
  
  (define srv-thd
    (thread
     (lambda ()
       (let accept-loop ()
         (define-values (in out) (tcp-accept listener))
         (thread (lambda () (handle-connection state in out)))
         (accept-loop)))))
  
  (server-instance listener port srv-thd state))

;; Graceful shutdown
(define (stop-vernier-server! inst)
  (tcp-close (server-instance-listener inst))
  (kill-thread (server-instance-server-thread inst))
  (storage-close! (server-state-storage (server-instance-state inst))))
