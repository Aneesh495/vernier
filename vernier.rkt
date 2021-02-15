#lang racket/base

;;; vernier.rkt
;;; Top-level executable entry point for Vernier CLI.

(require racket/cmdline
         "vernier/cli/main.rkt")

(run-cli (vector->list (current-command-line-arguments)))
