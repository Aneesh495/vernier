#lang racket/base

;;; vernier/query/lexer.rkt
;;; Lexical tokenizer for the Vernier Query Language (VQL).
;;; Scans filter expressions, duration literals, and boolean keywords.

(require racket/contract
         racket/string
         racket/format)

(provide
 (contract-out
  ;; Token structures
  [struct token ([type symbol?] [value any/c])]
  [tokenize-vql (-> string? (listof token?))]
  [parse-duration-literal (-> string? (or/c exact-integer? #f))]))

(struct token (type value) #:transparent)

;; Parses duration string into nanoseconds (e.g. "250ms" -> 250000000)
(define (parse-duration-literal str)
  (define m (regexp-match #px"^([0-9]+(?:\\.[0-9]+)?)(ns|us|µs|ms|s|m|h)$" str))
  (if m
      (let* ([num (string->number (cadr m))]
             [unit (caddr m)])
        (case unit
          [("ns") (inexact->exact (round num))]
          [("us" "µs") (inexact->exact (round (* num 1000.0)))]
          [("ms") (inexact->exact (round (* num 1000000.0)))]
          [("s") (inexact->exact (round (* num 1000000000.0)))]
          [("m") (inexact->exact (round (* num 60000000000.0)))]
          [("h") (inexact->exact (round (* num 3600000000000.0)))]
          [else #f]))
      #f))

;; Tokenizes a VQL query string into a list of tokens
(define (tokenize-vql input)
  (define len (string-length input))
  (let loop ([i 0] [tokens '()])
    (cond
      [(>= i len) (reverse tokens)]
      ;; Skip whitespace
      [(char-whitespace? (string-ref input i))
       (loop (add1 i) tokens)]
      ;; Parentheses
      [(char=? (string-ref input i) #\()
       (loop (add1 i) (cons (token 'lparen "(") tokens))]
      [(char=? (string-ref input i) #\))
       (loop (add1 i) (cons (token 'rparen ")") tokens))]
      ;; Colon
      [(char=? (string-ref input i) #\:)
       (loop (add1 i) (cons (token 'colon ":") tokens))]
      ;; Comparison operators: !=, <=, >=, =, <, >, ~
      [(and (< (add1 i) len) (string=? (substring input i (+ i 2)) "!="))
       (loop (+ i 2) (cons (token 'op '!=) tokens))]
      [(and (< (add1 i) len) (string=? (substring input i (+ i 2)) "<="))
       (loop (+ i 2) (cons (token 'op '<=) tokens))]
      [(and (< (add1 i) len) (string=? (substring input i (+ i 2)) ">="))
       (loop (+ i 2) (cons (token 'op '>=) tokens))]
      [(and (< (add1 i) len) (string=? (substring input i (+ i 2)) "&&"))
       (loop (+ i 2) (cons (token 'and "AND") tokens))]
      [(and (< (add1 i) len) (string=? (substring input i (+ i 2)) "||"))
       (loop (+ i 2) (cons (token 'or "OR") tokens))]
      [(char=? (string-ref input i) #\=)
       (loop (add1 i) (cons (token 'op '=) tokens))]
      [(char=? (string-ref input i) #\<)
       (loop (add1 i) (cons (token 'op '<) tokens))]
      [(char=? (string-ref input i) #\>)
       (loop (add1 i) (cons (token 'op '>) tokens))]
      [(char=? (string-ref input i) #\~)
       (loop (add1 i) (cons (token 'op '~) tokens))]
      [(char=? (string-ref input i) #\!)
       (loop (add1 i) (cons (token 'not "NOT") tokens))]
      ;; Quoted string: "..."
      [(char=? (string-ref input i) #\")
       (let str-loop ([j (add1 i)] [chars '()])
         (cond
           [(>= j len)
            (error 'tokenize-vql "Unterminated quoted string at position ~a" i)]
           [(char=? (string-ref input j) #\")
            (loop (add1 j) (cons (token 'string (list->string (reverse chars))) tokens))]
           [(and (char=? (string-ref input j) #\\) (< (add1 j) len))
            (str-loop (+ j 2) (cons (string-ref input (add1 j)) chars))]
           [else
            (str-loop (add1 j) (cons (string-ref input j) chars))]))]
      ;; Words, identifiers, duration literals, and keywords
      [else
       (let word-loop ([j i])
         (if (and (< j len)
                  (not (char-whitespace? (string-ref input j)))
                  (not (member (string-ref input j) (list #\( #\) #\: #\= #\< #\> #\~ #\" #\!))))
             (word-loop (add1 j))
             (let* ([word (substring input i j)]
                    [tok
                     (cond
                       [(string-ci=? word "AND") (token 'and "AND")]
                       [(string-ci=? word "OR") (token 'or "OR")]
                       [(string-ci=? word "NOT") (token 'not "NOT")]
                       [(parse-duration-literal word)
                        => (lambda (dur) (token 'duration dur))]
                       [(string->number word)
                        => (lambda (num) (token 'number num))]
                       [else (token 'ident word)])])
               (loop j (cons tok tokens)))))])))
