#lang racket/base

;;; vernier/protocol/otlp.rkt
;;; OpenTelemetry Protocol (OTLP/HTTP JSON) ingest and export adapters.
;;; Provides interoperability with OpenTelemetry collectors and Grafana agents.

(require racket/contract
         racket/list
         racket/string
         json
         "../core/id.rkt"
         "../core/span.rkt")

(provide
 (contract-out
  ;; OTLP JSON mapping
  [parse-otlp-json (-> (or/c string? bytes? hash?) (listof span?))]
  [spans->otlp-json (-> (listof span?) string? string?)]))

;; Decodes OTLP attribute value object: {"stringValue": "..."} | {"intValue": "..."}
(define (decode-otlp-value val-obj)
  (cond
    [(not (hash? val-obj)) val-obj]
    [(hash-has-key? val-obj 'stringValue) (hash-ref val-obj 'stringValue)]
    [(hash-has-key? val-obj "stringValue") (hash-ref val-obj "stringValue")]
    [(hash-has-key? val-obj 'intValue)
     (let ([v (hash-ref val-obj 'intValue)])
       (if (string? v) (string->number v) v))]
    [(hash-has-key? val-obj "intValue")
     (let ([v (hash-ref val-obj "intValue")])
       (if (string? v) (string->number v) v))]
    [(hash-has-key? val-obj 'boolValue) (hash-ref val-obj 'boolValue)]
    [(hash-has-key? val-obj "boolValue") (hash-ref val-obj "boolValue")]
    [(hash-has-key? val-obj 'doubleValue) (hash-ref val-obj 'doubleValue)]
    [(hash-has-key? val-obj "doubleValue") (hash-ref val-obj "doubleValue")]
    [else (format "~a" val-obj)]))

;; Parses an OTLP JSON document into native Vernier spans
(define (parse-otlp-json input)
  (define doc
    (cond
      [(string? input) (string->jsexpr input)]
      [(bytes? input) (string->jsexpr (bytes->string/utf-8 input))]
      [(hash? input) input]
      [else (error 'parse-otlp-json "Unsupported input format")]))
  
  (define res-spans (hash-ref doc 'resourceSpans (lambda () (hash-ref doc "resourceSpans" '()))))
  (define all-spans '())
  
  (for ([rs (in-list res-spans)])
    (define res (hash-ref rs 'resource (lambda () (hash-ref rs "resource" (hash)))))
    (define res-attrs (hash-ref res 'attributes (lambda () (hash-ref res "attributes" '()))))
    (define svc-name "unknown-service")
    (for ([attr (in-list res-attrs)])
      (define k (hash-ref attr 'key (lambda () (hash-ref attr "key" ""))))
      (when (string=? k "service.name")
        (set! svc-name (decode-otlp-value (hash-ref attr 'value (lambda () (hash-ref attr "value" "")))))))
    
    (define scope-spans (hash-ref rs 'scopeSpans (lambda () (hash-ref rs "scopeSpans" '()))))
    (for ([ss (in-list scope-spans)])
      (define spans (hash-ref ss 'spans (lambda () (hash-ref ss "spans" '()))))
      (for ([s (in-list spans)])
        (define tid (string-downcase (hash-ref s 'traceId (lambda () (hash-ref s "traceId")))))
        (define sid (string-downcase (hash-ref s 'spanId (lambda () (hash-ref s "spanId")))))
        (define raw-parent (hash-ref s 'parentSpanId (lambda () (hash-ref s "parentSpanId" #f))))
        (define pid (and raw-parent (not (string=? raw-parent "")) (string-downcase raw-parent)))
        (define name (hash-ref s 'name (lambda () (hash-ref s "name"))))
        
        (define start-raw (hash-ref s 'startTimeUnixNano (lambda () (hash-ref s "startTimeUnixNano"))))
        (define start-ns (if (string? start-raw) (string->number start-raw) start-raw))
        
        (define end-raw (hash-ref s 'endTimeUnixNano (lambda () (hash-ref s "endTimeUnixNano"))))
        (define end-ns (if (string? end-raw) (string->number end-raw) end-raw))
        
        ;; Extract attributes
        (define raw-attrs (hash-ref s 'attributes (lambda () (hash-ref s "attributes" '()))))
        (define attrs
          (for/hasheq ([a (in-list raw-attrs)])
            (define k (string->symbol (hash-ref a 'key (lambda () (hash-ref a "key")))))
            (define v (decode-otlp-value (hash-ref a 'value (lambda () (hash-ref a "value")))))
            (values k v)))
        
        ;; Status
        (define raw-status (hash-ref s 'status (lambda () (hash-ref s "status" (hash)))))
        (define status-code-val
          (hash-ref raw-status 'code (lambda () (hash-ref raw-status "code" 0))))
        (define status
          (case status-code-val
            [(1 "STATUS_CODE_OK") 'ok]
            [(2 "STATUS_CODE_ERROR") 'error]
            [else 'unset]))
        
        (define parsed-span
          (make-span #:trace-id tid
                     #:span-id sid
                     #:parent-id pid
                     #:name name
                     #:service-name svc-name
                     #:start-time start-ns
                     #:end-time end-ns
                     #:status-code status
                     #:attributes attrs))
        (set! all-spans (cons parsed-span all-spans)))))
  
  (reverse all-spans))

;; Serializes a list of Vernier spans into standard OTLP JSON string
(define (spans->otlp-json spans [service-name "vernier-service"])
  (define otlp-spans
    (map (lambda (s)
           (hash 'traceId (span-trace-id s)
                 'spanId (span-span-id s)
                 'parentSpanId (or (span-parent-id s) "")
                 'name (span-name s)
                 'kind 1
                 'startTimeUnixNano (number->string (span-start-time s))
                 'endTimeUnixNano (number->string (span-end-time s))
                 'attributes (for/list ([(k v) (in-hash (span-attributes s))])
                               (hash 'key (symbol->string k)
                                     'value (hash 'stringValue (format "~a" v))))
                 'status (hash 'code (case (span-status-code s)
                                       [(ok) 1]
                                       [(error) 2]
                                       [else 0]))))
         spans))
  
  (define doc
    (hash 'resourceSpans
          (list (hash 'resource (hash 'attributes (list (hash 'key "service.name"
                                                             'value (hash 'stringValue service-name))))
                      'scopeSpans (list (hash 'scope (hash 'name "vernier-exporter" 'version "0.1.0")
                                              'spans otlp-spans))))))
  (jsexpr->string doc))
