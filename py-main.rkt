#lang racket

(require (lib "eopl.ss" "eopl"))

(require "py-lexer.rkt"
         "py-grammar.rkt"
         "py-interpreter.rkt"
         "py-parser.rkt")

(provide (all-defined-out))

;evaluate -----------------------------------------------------------------------
(define (evaluate file-input-addr) (value-of
                                    (lex-and-parse
                                      (apply string-append
                                             (file->lines file-input-addr)))))

;(evaluate "tests/in4.txt")
;(evaluate "tests/in44.txt")
;(lex-and-parse (apply string-append (file->lines "tests/in44.txt")))
