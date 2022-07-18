#lang racket

(require (lib "eopl.ss" "eopl"))

(require "py-lexer.rkt"
         "py-grammar.rkt"
         "py-interpreter.rkt"
         "py-parser.rkt")

(provide (all-defined-out))

; Evaluate
(define (evaluate file-input-addr) (value-of
                                    (lex-and-parse
                                      (apply string-append
                                             (file->lines file-input-addr)))))