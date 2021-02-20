#lang racket/base

;;; tests/unit/test_racket_sdk.rkt
;;; Unit test suite for Racket Tracer SDK and with-span syntax form.

(require rackunit
         racket/list
         "../../vernier/core/id.rkt"
         "../../vernier/core/span.rkt"
         "../../vernier/sdk/racket/tracer.rkt")

(test-case "Manual span lifecycle with tracer"
  (define t (make-tracer "auth-service"))
  (define s1 (tracer-start-span t "authenticate-user" #:attributes (hash 'user.id 42)))
  (check-equal? (span-service-name s1) "auth-service")
  (check-equal? (span-name s1) "authenticate-user")
  (check-true (valid-span-id? (span-span-id s1)))
  (check-true (valid-trace-id? (span-trace-id s1)))
  
  (define child-span (tracer-start-span t "query-ldap" #:parent-span s1))
  (check-equal? (span-trace-id child-span) (span-trace-id s1))
  (check-equal? (span-parent-id child-span) (span-span-id s1))
  
  (tracer-finish-span! t child-span #:status 'ok)
  (tracer-finish-span! t s1 #:status 'ok)
  
  (define buffered (unbox (tracer-buffer-queue t)))
  (check-equal? (length buffered) 2))

(test-case "with-span macro block and nested context propagation"
  (define t (make-tracer "billing-engine"))
  (parameterize ([current-tracer t])
    (with-span ("process-invoice" #:attributes (hash 'invoice.id "inv-900"))
      (check-not-false (current-span))
      (check-equal? (span-name (current-span)) "process-invoice")
      
      ;; Nested child span automatically adopts parent
      (with-span ("charge-card" #:attributes (hash 'amount 5000))
        (check-equal? (span-name (current-span)) "charge-card")
        (check-pred string? (inject-w3c-traceparent (current-span))))))
  
  (define buffered (unbox (tracer-buffer-queue t)))
  (check-equal? (length buffered) 2)
  (define names (map span-name buffered))
  (check-true (and (member "process-invoice" names) #t))
  (check-true (and (member "charge-card" names) #t)))

(test-case "with-span exception capture"
  (define t (make-tracer "order-service"))
  (parameterize ([current-tracer t])
    (check-exn
     exn:fail?
     (lambda ()
       (with-span ("failing-operation")
         (error "database connection timeout")))))
  
  (define buffered (unbox (tracer-buffer-queue t)))
  (check-equal? (length buffered) 1)
  (define s (car buffered))
  (check-equal? (span-status-code s) 'error))
