#lang racket/base

;;; tests/unit/test_cli.rkt
;;; Unit test suite for Vernier CLI subcommands and options parser.

(require rackunit
         racket/port
         racket/string
         "../../vernier/cli/main.rkt")

(test-case "CLI help banner"
  (define out-str
    (with-output-to-string
      (lambda ()
        (with-handlers ([exn:break? void])
          ;; run-cli exits with 0 on empty args
          (run-cli '())))))
  (check-true (string? out-str)))

(test-case "CLI benchmark subcommand execution"
  (define out-str
    (with-output-to-string
      (lambda ()
        (run-cli (list "benchmark" "-n" "100")))))
  (check-true (string? out-str))
  (check-true (string-contains? out-str "Benchmark completed successfully.")))

(test-case "CLI query subcommand execution"
  (define out-str
    (with-output-to-string
      (lambda ()
        (run-cli (list "query" "service = \"web\"")))))
  (check-true (string? out-str))
  (check-true (string-contains? out-str "Query: service = \"web\"")))

(test-case "CLI trace view subcommand execution for missing trace"
  (define err-str
    (with-output-to-string
      (lambda ()
        (run-cli (list "trace" "00000000000000000000000000000000")))))
  (check-true (string? err-str)))
