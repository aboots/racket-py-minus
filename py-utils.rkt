#lang eopl

(require "py-env.rkt"
         "py-grammar.rkt")

(provide (all-defined-out))

;function datatype -----------------------------------------------------------------------
(define-datatype function function?
  (a-function
   (ID symbol?)
   (params (lambda (p) (or (null? p) (params? p))))
   (statements statements?)
   (scope scope?)))

(define add-args-to-scope
  (lambda (arg-list params scope thunk-scope)
    (if (null? arg-list)
        scope
        (cases param-with-default (car params)
          (a-param-with-default (ID-lhs exp)
                                (let ((ID (value-of-assignment-lhs ID-lhs scope)))
                              (add-args-to-scope
                                 (cdr arg-list)
                                 (cdr params)
                                 (extend-scope scope ID (a-thunk (car arg-list) thunk-scope))
                                 thunk-scope))
                                )))))

;expval datatype ----------------------------------------------------------------------------
(define expval?
  (lambda (e) (or (number? e) (boolean? e) (none? e) (function? e) (eval-list? e))))

;answer datatype -----------------------------------------------------------------------------
(define-datatype answer answer?
  (an-answer
   (value expval?)
   (message symbol?)
   (scope scope?)))

(define answer-val
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) val))))

(define answer-scope
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) sc))))

(define return-message?
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) (eqv? msg 'return)))))

(define continue-message?
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) (eqv? msg 'continue)))))

(define break-message?
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) (eqv? msg 'break)))))


;comparison answer ---------------------------------------------------------------
(define-datatype cmp-answer cmp-answer?
  (a-cmp-answer
   (result boolean?)
   (scope scope?)))   

(define value-of-assignment-lhs
  (lambda (ID-lhs scope)
    (cases assignment-lhs ID-lhs
      (assign-without-type (ID) ID)
      (assign-with-type (ID ty) ID))))


;thunk datatype -----------------------------------------------------------------------
(define-datatype thunk thunk?
  (a-thunk
   (exp expression?)
   (scope scope?)))

;evaluated list datatype -----------------------------------------------------------------------
(define-datatype eval-list eval-list?
  (an-eval-list
   (py-list py-list?)
   (scope scope?)))
