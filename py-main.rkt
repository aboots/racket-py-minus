#lang racket

(require (lib "eopl.ss" "eopl"))

(require "py-lexer.rkt"
         "py-grammar.rkt"
         "py-interpreter.rkt"
         "py-parser.rkt")

(provide (all-defined-out))

;evaluate -----------------------------------------------------------------------
(define (evaluate file-input-addr)
  (let ((ls (file->lines file-input-addr)))
        (let ((final_ls (cons (car ls) (cons " " (cdr ls)))))
     (value-of (lex-and-parse (apply string-append final_ls))))))

;(evaluate "tests/in4.txt")
;(evaluate "tests/in44.txt")
;(lex-and-parse (apply string-append (file->lines "tests/in44.txt")))
