#lang eopl

(require "py-env.rkt"
         "py-grammar.rkt")

(provide (all-defined-out))

;thunk datatype
(define-datatype thunk thunk?
  (a-thunk
   (exp expression?)
   (scope scope?)))

;evaluated list datatype
(define-datatype eval-list eval-list?
  (an-eval-list
   (py-list py-list?)
   (scope scope?)))
