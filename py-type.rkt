#lang racket
(require eopl)

(require "py-env.rkt"
         "py-grammar.rkt"
         "py-utils.rkt")

(provide (all-defined-out))

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record (sym symbol?)
                        (type type?)
                        (tenv type-environment?)))

(define-datatype procedure-type procedure-type?
  (proc-type
   (args-type (list-of type?))
   (result-type type?)))

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      [empty-tenv-record () (eopl:error 'apply-tenv "Unbound variable ~s" sym)]
      [extended-tenv-record (sym1 val1 old-env) (if (eqv? sym sym1)
                                                    val1
                                                    (apply-tenv old-env sym))])))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define extend-tenv*
  (lambda (syms types saved-tenv)
    (if (null? syms)
        saved-tenv
        (extend-tenv* (cdr syms)
                      (cdr types)
                      (extend-tenv (car syms)
                                   (car types)
                                   saved-tenv)))))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      [int-type () 'int]
      [float-type () 'float]
      [bool-type () 'bool]
      [none-type () 'none]
      [list-type () 'list])))

(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
                "Types didn't match: ~s != ~a in~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp))))

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
                "Rator not a proc type:~%~s~%had rator type ~s"
                rator
                (type-to-external-form rator-type))))

(define report-rator-not-a-list-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
                "Rator not a list type:~%~s~%had rator type ~s"
                rator
                (type-to-external-form rator-type))))


(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (statements) (none-type))
      (checked-program (statements) (type-of-statements statements empty-tenv)))))

(define type-of-statements
  (lambda (sts scope)
    (cases statements sts
      (mult-statements (sts st)
                       (let loop ([exps sts]
                                  [st-type (none-type)])
                         (if (null? exps)
                             (type-of-statement st scope)
                             (let ([exp (car exps)])
                               (loop (cdr exps) (type-of-statement exp scope))))))
      (single-statements (st) (type-of-statement st scope)))))


(define type-of-statement
  (lambda (st scope)
    (cases statement st
      (a-compound-stmt (st) (type-of-compound-stmt st scope))
      (a-simple-stmt (st) (type-of-simple-stmt st scope))
      (a-print-stmt (st) (type-of-print st scope)))))

(define type-of-simple-stmt
  (lambda (st scope)
    (cases simple-stmt st
      (assignment-simple-stmt (assignment) (type-of-assignment assignment scope))
      (return-simple-stmt (return-stmt) (type-of-return-stmt return-stmt scope))
      (global-simple-stmt (global-stmt)(type-of-global-stmt global-stmt scope))
      (pass-stmt () (none-type))
      (break-stmt () (none-type))
      (continue-stmt () (none-type)))))

(define type-of-assignment
  (lambda (as scope)
    (cases assignment as
      (an-assignment (ID-lhs exp)
                     (let ([var-type (cadr (exp->assignment-lhs ID-lhs))]
                           [ID (car (exp->assignment-lhs ID-lhs))]
                           [result-type (type-of-expression exp scope)])
                       (check-equal-type! var-type result-type exp)
                       (extend-scope scope ID result-type)
                       (none-type))))))

(define type-of-return-stmt
  (lambda (return-st scope)
    (cases return-stmt return-st
      (empty-return-stmt () (none-type))
      (exp-return-stmt (exp)
                       (let ((ty (type-of-print exp scope)))
                         (none-type))))))

(define type-of-global-stmt
  (lambda (global-st scope)
    (cases global-stmt global-st
      (a-global-stmt (ID)
                     (none-type)))))
                          
(define type-of-compound-stmt
  (lambda (st scope)
    (cases compound-stmt st
      (func-def-comp-stmt (func-def) (type-of-func-def func-def scope))
      (if-comp-stmt (if-stmt) (type-of-if-stmt if-stmt scope))
      (for-comp-stmt (for-stmt) (type-of-for-stmt for-stmt scope)))))

(define type-of-func-def
  (lambda (func-def scope)
    (cases function-def func-def
      (params-func-def (ID params t-result sts)
                       (let ([vars (list "must fill")]
                             [param-types (list "must fill")])
                         (let ([result-type (type-of-statements sts (extend-tenv* vars param-types scope))])
                           (check-equal-type! t-result result-type sts)
                           (proc-type param-types result-type))))
      (zero-param-func-def (ID t-result sts)
                           (let ([result-type (type-of-statements sts)])
                             (check-equal-type! t-result result-type sts)
                             (proc-type (none-type) result-type))))))

(define type-of-if-stmt
  (lambda (if-st scope)
    (cases if-stmt if-st
      (an-if-stmt (exp sts else-block)
                  (let ([ty1 (type-of-expression exp scope)]
                        [ty2 (type-of-statements sts scope)]
                        [ty3 (type-of-else-block else-block scope)])
                    (check-equal-type! ty1 (bool-type) exp)
                    (check-equal-type! ty2 ty3 exp)
                    ty2)))))

(define type-of-else-block
  (lambda (else-bl scope)
    (cases else-block else-bl
      (an-else-block (sts)
                     (type-of-statements sts scope)))))

(define type-of-for-stmt
  (lambda (for-st scope)
    (cases for-stmt for-st
      (a-for-stmt (ID exp sts)
                  (let loop ([lst sts])
                    (if (null? lst)
                        (none-type)
                        (let ([dummy (type-of-statements (extend-tenv ID (type-of-expression (car lst) scope)))]) 
                          (loop (cdr lst)))))))))

(define type-of-expression
  (lambda (exp scope)
    (cases expression exp
      (an-expression (disj)
                     (type-of-disjunction disj scope)))))

(define type-of-disjunction
  (lambda (disj scope)
    (cases disjunction disj
      (single-disjunction (conj)
                          (type-of-conjuction conj scope))
      (mult-disjunction (disj conj)
                        (let ([ty1 (type-of-disjunction disj scope)]
                              [ty2 (type-of-conjuction conj scope)])
                          (check-equal-type! ty1 (bool-type) disj)
                          (check-equal-type! ty2 (bool-type) conj)
                          (bool-type))))))
(define type-of-conjuction
  (lambda (conj scope)
    (cases conjunction conj
      (single-conjunction (inv)
                          (type-of-inversion inv scope))
      (mult-conjunction (conj inv)
                        (let ([ty1 (type-of-conjuction conj scope)]
                              [ty2 (type-of-inversion inv scope)])
                          (check-equal-type! ty1 (bool-type) conj)
                          (check-equal-type! ty2 (bool-type) inv)
                          (bool-type))))))

(define type-of-inversion
  (lambda (inv scope)
    (cases inversion inv
      (not-inversion (inv)
                     (let ([ty1 (type-of-inversion inv scope)])
                       (check-equal-type! ty1 (bool-type) inv)
                       (bool-type)))
      (a-comparison (comp)
                    (type-of-comparison comp scope)))))

(define type-of-comparison
  (lambda (comp scope)
    (cases comparison comp
      (single-comparison (sum)
                         (type-of-sum sum scope))
      (eq-sum-comparison (eq-sum)
                         (type-of-eq-sum eq-sum scope))
      (lt-sum-comparison (lt-sum)
                         (type-of-lt-sum lt-sum scope))
      (gt-sum-comparison (gt-sum)
                         (type-of-gt-sum gt-sum scope)))))

(define type-of-eq-sum
  (lambda (eq-s scope)
    (cases eq-sum eq-s
      (an-eq-sum (sum1 sum2)
                 (let ([ty1 (type-of-sum sum1 scope)]
                       [ty2 (type-of-sum sum2 scope)])
                   (if (or
                        (and (equal? ty1 (int-type)) (equal? ty2 (int-type)))
                        (and (equal? ty1 (bool-type)) (equal? ty2 (bool-type))))
                       (check-equal-type! ty1 ty2 sum1) ; dummy
                       (report-unequal-types ty1 ty2 sum1))
                   (bool-type))))))

(define type-of-lt-sum
  (lambda (lt-s scope)
    (cases lt-sum lt-s
      (an-lt-sum (sum1 sum2)
                 (let ([ty1 (type-of-sum sum1 scope)]
                       [ty2 (type-of-sum sum2 scope)])
                   (if (and (equal? ty1 (int-type)) (equal? ty2 (int-type)))
                       (check-equal-type! ty1 ty2 sum1) ; dummy
                       (report-unequal-types ty1 ty2 sum1))
                   (bool-type))))))

(define type-of-gt-sum
  (lambda (gt-s scope)
    (cases gt-sum gt-s
      (a-gt-sum (sum1 sum2)
                (let ([ty1 (type-of-sum sum1 scope)]
                      [ty2 (type-of-sum sum2 scope)])
                  (if (and (equal? ty1 (int-type)) (equal? ty2 (int-type)))
                      (check-equal-type! ty1 ty2 sum1) ; dummy
                      (report-unequal-types ty1 ty2 sum1))
                  (bool-type))))))


(define type-of-sum
  (lambda (s scope)
    (cases sum s
      (add-sum (sum term)
               (let ([ty1 (type-of-sum sum scope)]
                     [ty2 (type-of-term term scope)])
                 (if (or
                      (and (equal? ty1 (int-type)) (equal? ty2 (int-type)))
                      (and (equal? ty1 (bool-type)) (equal? ty2 (bool-type))))
                     (check-equal-type! ty1 ty2 sum) ; dummy
                     (report-unequal-types ty1 ty2 sum))
                 ty1))
      (sub-sum (sum term)
               (let ([ty1 (type-of-sum sum scope)]
                     [ty2 (type-of-term term scope)])
                 (check-equal-type! ty1 (int-type) sum)
                 (check-equal-type! ty2 (int-type) term)
                 (int-type)))
      (single-sum (term)
                  (type-of-term term scope)))))

(define type-of-term
  (lambda (t scope)
    (cases term t
      (mul-term (term factor)
                (let ([ty1 (type-of-term term scope)]
                      [ty2 (type-of-factor factor scope)])
                  (if (or
                       (and (equal? ty1 (int-type)) (equal? ty2 (int-type)))
                       (and (equal? ty1 (bool-type)) (equal? ty2 (bool-type))))
                      (check-equal-type! ty1 ty2 term) ; dummy
                      (report-unequal-types ty1 ty2 term))
                  ty1))
      (div-term (term factor)
                (let ([ty1 (type-of-term term scope)]
                      [ty2 (type-of-factor factor scope)])
                  (check-equal-type! ty1 (int-type) term)
                  (check-equal-type! ty2 (int-type) factor)
                  (int-type)))
      (single-term (factor)
                   (type-of-factor factor scope)))))

(define type-of-factor
  (lambda (fact scope)
    (cases factor fact
      (pos-factor (pow)
                  (let ([ty1 (type-of-power pow scope)])
                    (check-equal-type! ty1 (int-type) pow)
                    (int-type)))
      (neg-factor (pow)
                  (let ([ty1 (type-of-power pow scope)])
                    (check-equal-type! ty1 (int-type) pow)
                    (int-type)))
      (single-factor (pow)
                     (type-of-power pow scope)))))

(define type-of-power
  (lambda (pow scope)
    (cases power pow
      (a-pow (atom factor)
             (let ([ty1 (type-of-atom atom scope)]
                   [ty2 (type-of-factor factor scope)])
               (check-equal-type! ty1 (int-type) atom)
               (check-equal-type! ty2 (int-type) factor)
               (int-type)))
      (a-primary (primary)
                 (type-of-primary primary scope)))))

(define type-of-primary
  (lambda (prim scope)
    (cases primary prim
      (an-atom (atom)
               (type-of-atom atom scope))
      (index-access (primary exp)
                    (let ([idx-type (type-of-expression exp scope)])
                      (check-equal-type! idx-type (int-type) exp)
                      (none-type))) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; must be type of element of list
      (zero-arg-func-call (primary)
                          (type-of-primary primary scope))
      (args-func-call (primary args)
                      (let ([rator-type (type-of-primary primary scope)]
                            [rand-types (map (lambda (arg)
                                               (type-of-argument arg scope))
                                             args)])
                        (if (procedure-type? rator-type)
                         (cases procedure-type rator-type
                          [proc-type (arg-types result-type) (begin (for-each check-equal-type!
                                                                              arg-types
                                                                              rand-types
                                                                              args)
                                                                    result-type)])
                          (report-rator-not-a-proc-type rator-type primary)))))))


(define type-of-atom
  (lambda (atom scope)
    (cond
      ((symbol? atom) ((apply-tenv scope atom)))
      ((py-list? atom) (list-type))
      ((boolean? atom) (bool-type))
      ((none? atom) (none-type))
      ((integer? atom) (int-type))
      ((flonum? atom) (float-type))
      )))

(define type-of-argument
  (lambda (atom scope)
    (list-type)))


(define type-of-param-with-default
  (lambda (pwd scope)
    (cases param-with-default pwd
      (a-param-with-default (ID exp)
                            (none-type)))))

(define type-of-print
  (lambda (pr scope)
    (cases print pr
      (print-exp (items)
                  (none-type)))))

