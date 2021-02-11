#lang racket/base

;;; vernier/server/router.rkt
;;; Lightweight HTTP request router, query string parser, and response builder.
;;; Dispatches REST endpoints and handles CORS negotiation with zero external dependencies.

(require racket/contract
         racket/string
         racket/list
         json)

(provide
 (contract-out
  ;; HTTP request and response structures
  [struct http-request ([method string?]
                        [path string?]
                        [query-params hash?]
                        [headers hash?]
                        [body bytes?])]
  
  [struct http-response ([status-code exact-positive-integer?]
                         [headers hash?]
                         [body bytes?])]
  
  [make-http-response (->* (exact-positive-integer?) (#:headers hash? #:body (or/c bytes? string? hash? list?)) http-response?)]
  [response-json (->* (any/c) (#:status exact-positive-integer?) http-response?)]
  [response-not-found (-> http-response?)]
  [response-bad-request (->* (string?) () http-response?)]
  [parse-query-string (-> string? hash?)]
  [read-http-request (-> input-port? (or/c http-request? #f))]
  [write-http-response (-> http-response? output-port? void?)]))

(struct http-request (method path query-params headers body)
  #:transparent)

(struct http-response (status-code headers body)
  #:transparent)

;; Constructs generic HTTP response
(define (make-http-response status #:headers [headers (hash)] #:body [body #""])
  (define body-bytes
    (cond
      [(bytes? body) body]
      [(string? body) (string->bytes/utf-8 body)]
      [(or (hash? body) (list? body)) (string->bytes/utf-8 (jsexpr->string body))]
      [else (string->bytes/utf-8 (format "~a" body))]))
  
  (define base-headers
    (hash "content-length" (number->string (bytes-length body-bytes))
          "access-control-allow-origin" "*"
          "access-control-allow-methods" "GET, POST, OPTIONS"
          "access-control-allow-headers" "Content-Type, traceparent, tracestate"
          "connection" "close"))
  
  (define merged-headers
    (for/fold ([acc base-headers]) ([(k v) (in-hash headers)])
      (hash-set acc (string-downcase (format "~a" k)) (format "~a" v))))
  
  (http-response status merged-headers body-bytes))

;; Constructs JSON response
(define (response-json data #:status [status 200])
  (make-http-response status
                      #:headers (hash "content-type" "application/json")
                      #:body data))

(define (response-not-found)
  (response-json (hash 'error "Not Found") #:status 404))

(define (response-bad-request msg)
  (response-json (hash 'error msg) #:status 400))

;; Parses URL query string: "service=gateway&limit=10" -> hash
(define (parse-query-string qs)
  (if (or (string=? qs "") (not qs))
      (hash)
      (let ([pairs (string-split qs "&")])
        (for/fold ([acc (hash)]) ([pair (in-list pairs)])
          (define kv (string-split pair "="))
          (if (= (length kv) 2)
              (hash-set acc (car kv) (cadr kv))
              (if (= (length kv) 1)
                  (hash-set acc (car kv) #t)
                  acc))))))

;; Reads HTTP request from port
(define (read-http-request in)
  (define req-line (read-line in 'any))
  (if (or (eof-object? req-line) (string=? req-line ""))
      #f
      (let* ([parts (string-split (string-trim req-line) " ")])
        (if (< (length parts) 2)
            #f
            (let* ([method (string-upcase (car parts))]
                   [full-uri (cadr parts)]
                   [uri-parts (string-split full-uri "?")]
                   [path (car uri-parts)]
                   [query-params (if (> (length uri-parts) 1)
                                     (parse-query-string (cadr uri-parts))
                                     (hash))]
                   ;; Read headers
                   [headers
                    (let loop ([h (hash)])
                      (define line (read-line in 'any))
                      (if (or (eof-object? line) (string=? (string-trim line) ""))
                          h
                          (let ([colon-pos (string-find line ":")])
                            (if colon-pos
                                (let ([k (string-downcase (string-trim (substring line 0 colon-pos)))]
                                      [v (string-trim (substring line (add1 colon-pos)))])
                                  (loop (hash-set h k v)))
                                (loop h)))))]
                   ;; Read body if Content-Length present
                   [content-len (string->number (hash-ref headers "content-length" "0"))]
                   [body (if (and content-len (> content-len 0))
                             (read-bytes content-len in)
                             #"")])
              (http-request method path query-params headers (if (eof-object? body) #"" body)))))))

(define (string-find str sub)
  (let loop ([i 0])
    (cond
      [(> (+ i (string-length sub)) (string-length str)) #f]
      [(string=? (substring str i (+ i (string-length sub))) sub) i]
      [else (loop (add1 i))])))

;; Status text mapping
(define (status-phrase code)
  (case code
    [(200) "OK"]
    [(201) "Created"]
    [(202) "Accepted"]
    [(204) "No Content"]
    [(400) "Bad Request"]
    [(404) "Not Found"]
    [(500) "Internal Server Error"]
    [else "Unknown"]))

;; Writes HTTP response to port
(define (write-http-response resp out)
  (define code (http-response-status-code resp))
  (fprintf out "HTTP/1.1 ~a ~a\r\n" code (status-phrase code))
  (for ([(k v) (in-hash (http-response-headers resp))])
    (fprintf out "~a: ~a\r\n" k v))
  (fprintf out "\r\n")
  (write-bytes (http-response-body resp) out)
  (flush-output out))
