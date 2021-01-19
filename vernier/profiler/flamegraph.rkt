#lang racket/base

;;; vernier/profiler/flamegraph.rkt
;;; Flamegraph synthesis, collapsed stack format, and hierarchical JSON trees.
;;; Aggregates execution time profiles across distributed call hierarchies.

(require racket/contract
         racket/list
         racket/string
         racket/format
         "../core/span.rkt"
         "../core/trace.rkt"
         "../dag/graph.rkt")

(provide
 (contract-out
  ;; Flamegraph tree node
  [struct flame-node ([name string?]
                      [service-name string?]
                      [value-nanos exact-integer?]
                      [children (listof flame-node?)])]
  
  [trace->flamegraph-tree (-> trace-record? flame-node?)]
  [trace->collapsed-stacks (-> trace-record? (listof string?))]
  [render-flamegraph-svg (->* (trace-record?) (#:width exact-positive-integer? #:row-height exact-positive-integer?) string?)]))

(struct flame-node (name service-name value-nanos children)
  #:transparent)

;; Converts a trace into a hierarchical flame-node tree
(define (trace->flamegraph-tree tr)
  (define g (trace->dag tr))
  (define root (trace-record-root-span tr))
  (if (not root)
      (flame-node "root" "unknown" 0 '())
      (let build-node ([sid (span-span-id root)])
        (define s (trace-find-span tr sid))
        (define child-ids (hash-ref (trace-dag-adj-out g) sid '()))
        (define child-nodes
          (map build-node child-ids))
        (flame-node (span-name s)
                    (span-service-name s)
                    (span-duration s)
                    child-nodes))))

;; Generates Brendan Gregg collapsed stack lines: "svcA;svcB;op 150000000"
(define (trace->collapsed-stacks tr)
  (define g (trace->dag tr))
  (define root (trace-record-root-span tr))
  (if (not root)
      '()
      (let walk ([sid (span-span-id root)] [stack-prefix '()])
        (define s (trace-find-span tr sid))
        (define current-label (format "~a:~a" (span-service-name s) (span-name s)))
        (define current-stack (append stack-prefix (list current-label)))
        (define child-ids (hash-ref (trace-dag-adj-out g) sid '()))
        (if (null? child-ids)
            (list (format "~a ~a" (string-join current-stack ";") (span-duration s)))
            (let* ([child-lines (apply append (map (lambda (cid) (walk cid current-stack)) child-ids))]
                   ;; Local self time = span duration - sum of child durations
                   [child-dur-sum (for/sum ([cid (in-list child-ids)])
                                    (span-duration (trace-find-span tr cid)))]
                   [self-dur (max 0 (- (span-duration s) child-dur-sum))]
                   [self-line (if (> self-dur 0)
                                  (list (format "~a;[self] ~a" (string-join current-stack ";") self-dur))
                                  '())])
              (append self-line child-lines))))))

;; Deterministic color generator for service names
(define (service->color svc-name)
  (define colors (vector "#4a90e2" "#50e3c2" "#f5a623" "#e06377" "#9013fe" "#7ed321" "#bd10e0"))
  (define hash-code (for/sum ([c (in-string svc-name)]) (char->integer c)))
  (vector-ref colors (modulo hash-code (vector-length colors))))

;; Renders an interactive SVG flamegraph document
(define (render-flamegraph-svg tr #:width [svg-width 1000] #:row-height [row-h 24])
  (define tree (trace->flamegraph-tree tr))
  (define total-dur (max 1 (flame-node-value-nanos tree)))
  (define rects '())
  
  (define (render-box node depth x-start width-px)
    (define dur (flame-node-value-nanos node))
    (define svc (flame-node-service-name node))
    (define name (flame-node-name node))
    (define color (service->color svc))
    (define y-pos (* depth (+ row-h 4)))
    
    (define label (format "~a (~a)" name svc))
    (define rect-svg
      (format "<g class=\"flame-bar\" transform=\"translate(~a, ~a)\">\n  <rect width=\"~a\" height=\"~a\" fill=\"~a\" rx=\"3\" stroke=\"#ffffff\" stroke-width=\"1\"/>\n  <text x=\"6\" y=\"16\" font-family=\"monospace\" font-size=\"11\" fill=\"#111111\">~a</text>\n</g>"
              (/ (round (* x-start 10.0)) 10.0)
              y-pos
              (max 2.0 (/ (round (* width-px 10.0)) 10.0))
              row-h
              color
              (if (< width-px 40) "" label)))
    (set! rects (cons rect-svg rects))
    
    ;; Render children proportionally
    (define child-x x-start)
    (for ([c (in-list (flame-node-children node))])
      (define c-dur (flame-node-value-nanos c))
      (define c-width (* width-px (/ c-dur (* 1.0 (max 1 dur)))))
      (render-box c (add1 depth) child-x c-width)
      (set! child-x (+ child-x c-width))))
  
  (render-box tree 0 10.0 (- svg-width 20.0))
  
  (define total-height (* 12 (+ row-h 4)))
  (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~a\" height=\"~a\" viewBox=\"0 0 ~a ~a\">\n<style>\n  .flame-bar:hover rect { filter: brightness(1.15); cursor: pointer; }\n</style>\n~a\n</svg>"
          svg-width total-height svg-width total-height
          (string-join (reverse rects) "\n")))
