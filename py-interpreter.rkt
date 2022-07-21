#lang racket
(require eopl)

(require "py-env.rkt"
         "py-grammar.rkt"
         "py-utils.rkt"
         "py-parser.rkt")

(provide (all-defined-out))

;Value of  -----------------------------------------------------------------------
(define value-of
  (lambda (pgm)
    (cases program pgm
      (a-program (statements)(begin
                               (value-of-statements statements (new-global-scope))
                               (display "")))
      (checked-program (ch statements) (begin
                                         (value-of-statements statements (new-global-scope))
                                         (display ""))))))

(define value-of-statements
  (lambda (sts scope)
    (cases statements sts
      (mult-statements (sts st)
                       (let ((ans (value-of-statements sts scope)))
                         (if (not (or (return-message? ans) (break-message? ans) (continue-message? ans)))
                             (value-of-statement st (answer-scope ans))
                             ans)))
      (single-statements (st)
                        (value-of-statement st scope)))))

(define value-of-statement
  (lambda (st scope)
    (cases statement st
      (a-compound-stmt (st) (value-of-compound-stmt st scope))
      (a-simple-stmt (st) (value-of-simple-stmt st scope))
      (a-print-stmt (st) (value-of-print st scope)))))

(define value-of-simple-stmt
  (lambda (st scope)
    (cases simple-stmt st
      (assignment-simple-stmt (assignment) (value-of-assignment assignment scope))
      (return-simple-stmt (return-stmt) (value-of-return-stmt return-stmt scope))
      (global-simple-stmt (global-stmt)(value-of-global-stmt global-stmt scope))
      (pass-stmt () (an-answer (a-none) '- scope))
      (break-stmt () (an-answer (a-none) 'break scope))
      (continue-stmt () (an-answer (a-none) 'continue scope)))))

(define value-of-assignment
  (lambda (as scope)
    (cases assignment as
      (an-assignment (ID-lhs exp)
                     (let ((ID (value-of-assignment-lhs ID-lhs scope)))
                             (an-answer (a-none) '- (extend-scope scope ID (a-thunk exp (copy-of-scope scope)))))))))

(define value-of-return-stmt
        (lambda (return-st scope)
          (cases return-stmt return-st
            (empty-return-stmt () (an-answer (a-none) 'return scope))
            (exp-return-stmt (exp)
                             (let ((ans (value-of-expression exp scope)))
                               (an-answer (answer-val ans) 'return (answer-scope ans)))))))

(define value-of-global-stmt
  (lambda (global-st scope)
    (cases global-stmt global-st
      (a-global-stmt (ID)
                     (an-answer (a-none) '- (add-to-global-var-list scope ID))))))
                                 
(define value-of-compound-stmt
  (lambda (st scope)
    (cases compound-stmt st
      (func-def-comp-stmt (func-def) (value-of-function-def func-def scope))
      (if-comp-stmt (if-stmt) (value-of-if-stmt if-stmt scope))
      (for-comp-stmt (for-stmt) (value-of-for-stmt for-stmt scope)))))

(define value-of-function-def
  (lambda (func-def scope)
    (cases function-def func-def
      (params-func-def (ID params return-type sts)
                           (let ((new-func (a-function ID params sts (new-local-scope scope))))
                             (an-answer (a-none) '- (extend-scope scope ID new-func))))
      (zero-param-func-def (ID return-type sts)
                           (let ((new-func (a-function ID (list) sts (new-local-scope scope))))
                             (an-answer (a-none) '- (extend-scope scope ID new-func)))))))

(define value-of-if-stmt
  (lambda (if-st scope)
    (cases if-stmt if-st
      (an-if-stmt (exp sts else-block)
                  (let ((ans (value-of-expression exp scope)))
                    (if (answer-val ans)
                        (value-of-statements sts (answer-scope ans))
                        (value-of-else-block else-block (answer-scope ans))))))))

(define value-of-else-block
 (lambda (else-bl scope)
   (cases else-block else-bl
     (an-else-block (sts)
                    (value-of-statements sts scope)))))

(define value-of-for-stmt
  (lambda (for-st scope)
    (cases for-stmt for-st
      (a-for-stmt (ID exp sts)
                  (let ((ans (value-of-expression exp scope)))
                    (cases eval-list (answer-val ans)
                      (an-eval-list (py-list sc)
                                    (value-of-for-bodies ID py-list sc sts (answer-scope ans)))))))))

(define value-of-for-bodies
  (lambda (ID iterable stored-scope sts scope)
    (if (null? iterable)
        (an-answer (a-none) '- scope)
        (let ((ans1 (value-of-expression (car iterable) stored-scope)))
          (let ((ans2 (value-of-statements sts (extend-scope scope ID (answer-val ans1)))))
            (if (break-message? ans2)
                (an-answer (a-none) '- (answer-scope ans2))
                (value-of-for-bodies ID (cdr iterable) stored-scope sts (answer-scope ans2))))))))
                  
(define value-of-expression
  (lambda (exp scope)
    (cases expression exp
      (an-expression (disj)
                     (value-of-disjunction disj scope)))))

(define value-of-disjunction
  (lambda (disj scope)
    (cases disjunction disj
      (single-disjunction (conj)
                          (value-of-conjunction conj scope))
      (mult-disjunction (disj conj)
                        (let ((ans1 (value-of-disjunction disj scope)))
                          (let ((ans2 (value-of-conjunction conj (answer-scope ans1))))
                            (an-answer (or (answer-val ans1) (answer-val ans2)) '- (answer-scope ans2))))))))

(define value-of-conjunction
  (lambda (conj scope)
    (cases conjunction conj
      (single-conjunction (inv)
                          (value-of-inversion inv scope))
      (mult-conjunction (conj inv)
                        (let ((ans1 (value-of-conjunction conj scope)))
                          (let ((ans2 (value-of-inversion inv (answer-scope ans1))))
                            (an-answer (and (answer-val ans1) (answer-val ans2)) '- (answer-scope ans2))))))))

(define value-of-inversion
  (lambda (inv scope)
    (cases inversion inv
      (not-inversion (inv)
                     (let ((ans (value-of-inversion inv scope)))
                       (an-answer (not (answer-val ans)) '- (answer-scope ans))))
      (a-comparison (comp)
                    (value-of-comparison comp scope)))))

(define value-of-comparison
  (lambda (comp scope)
    (cases comparison comp
      (single-comparison (sum)
                         (value-of-sum sum scope))
      (eq-sum-comparison (eq-sum)
                         (let ((cmp-ans (value-of-eq-sum eq-sum scope)))
                           (cases cmp-answer cmp-ans
                             (a-cmp-answer (res sc) (an-answer res '- sc)))))
      (lt-sum-comparison (lt-sum)
                         (let ((cmp-ans (value-of-lt-sum lt-sum scope)))
                           (cases cmp-answer cmp-ans
                             (a-cmp-answer (res sc) (an-answer res '- sc)))))
      (gt-sum-comparison (gt-sum)
                         (let ((cmp-ans (value-of-gt-sum gt-sum scope)))
                           (cases cmp-answer cmp-ans
                             (a-cmp-answer (res sc) (an-answer res '- sc))))))))

(define value-of-eq-sum
  (lambda (eq-s scope)
    (cases eq-sum eq-s
      (an-eq-sum (sum1 sum2)
                 (let ((ans1 (value-of-sum sum1 scope))
                       (ans2 (value-of-sum sum2 scope)))
                   (a-cmp-answer(= (answer-val ans1) (answer-val ans2)) (answer-scope ans2)))))))
          
(define value-of-lt-sum
  (lambda (lt-s scope)
    (cases lt-sum lt-s
      (an-lt-sum (sum1 sum2)
                 (let ((ans1 (value-of-sum sum1 scope))
                       (ans2 (value-of-sum sum2 scope)))
                   (a-cmp-answer(< (answer-val ans1) (answer-val ans2)) (answer-scope ans2)))))))

(define value-of-gt-sum
  (lambda (gt-s scope)
    (cases gt-sum gt-s
      (a-gt-sum (sum1 sum2)
                (let ((ans1 (value-of-sum sum1 scope))
                      (ans2 (value-of-sum sum2 scope)))
                  (a-cmp-answer (> (answer-val ans1) (answer-val ans2)) (answer-scope ans2)))))))

(define value-of-sum
  (lambda (s scope)
    (cases sum s
      (add-sum (sum term)
               (let ((ans1 (value-of-sum sum scope)))
                 (let ((ans2 (value-of-term term (answer-scope ans1))))
                   (let ((exp-val1 (answer-val ans1))
                         (exp-val2 (answer-val ans2))
                         (scope (answer-scope ans2)))
                     (cond
                       ((boolean? exp-val1) (an-answer (or exp-val1 exp-val2) '- scope))
                       ((eval-list? exp-val1)
                        (cases eval-list exp-val1
                          (an-eval-list (py-list1 sc1)
                                        (cases eval-list exp-val2
                                          (an-eval-list (py-list2 sc2)
                                                        (an-answer (an-eval-list (append py-list1 py-list2) scope) '- scope))))))
                       (else (an-answer (+ exp-val1 exp-val2) '- scope)))))))
      (sub-sum (sum term)
               (let ((ans1 (value-of-sum sum scope)))
                 (let ((ans2 (value-of-term term (answer-scope ans1))))
                   (let ((exp-val1 (answer-val ans1))
                         (exp-val2 (answer-val ans2))
                         (scope (answer-scope ans2)))
                     (an-answer (- exp-val1 exp-val2) '- scope)))))
      (single-sum (term)
                  (value-of-term term scope)))))

(define value-of-term
  (lambda (t scope)
    (cases term t
      (mul-term (term factor)
               (let ((ans1 (value-of-term term scope)))
                 (let ((exp-val1 (answer-val ans1))
                       (scope (answer-scope ans1)))
                   (if (boolean? exp-val1)
                       (if (not exp-val1)
                           (an-answer #f '- scope)
                           (let ((ans2 (value-of-factor factor scope)))
                             (an-answer (and exp-val1 (answer-val ans2)) '- (answer-scope ans2))))
                       (if (zero? exp-val1)
                           (an-answer 0 '- scope)
                           (let ((ans2 (value-of-factor factor scope)))
                             (an-answer (* exp-val1 (answer-val ans2)) '- (answer-scope ans2))))))))
      (div-term (term factor)
               (let ((ans1 (value-of-term term scope)))
                 (let ((ans2 (value-of-factor factor (answer-scope ans1))))
                   (an-answer (real->double-flonum (/ (answer-val ans1) (answer-val ans2))) '- (answer-scope ans2)))))
      (single-term (factor)
                   (value-of-factor factor scope)))))

(define value-of-factor
  (lambda (fact scope)
    (cases factor fact
      (pos-factor (pow)
                  (let ((ans (value-of-power pow scope)))
                    (an-answer (answer-val ans) '- (answer-scope ans))))
      (neg-factor (pow)
                  (let ((ans (value-of-power pow scope)))
                    (an-answer (- (answer-val ans)) '- (answer-scope ans))))
      (single-factor (pow)
                     (value-of-power pow scope)))))

(define value-of-power
  (lambda (pow scope)
    (cases power pow
      (a-pow (atom factor)
             (let ((ans1 (value-of-atom atom scope)))
               (let ((ans2 (value-of-factor factor (answer-scope ans1))))
                 (an-answer (expt (answer-val ans1) (answer-val ans2)) '- (answer-scope ans2)))))
      (a-primary (primary)
                 (value-of-primary primary scope)))))

(define value-of-primary
  (lambda (prim scope)
    (cases primary prim
      (an-atom (atom)
               (value-of-atom atom scope))
      (index-access (primary exp)
                   (let ((ans1 (value-of-primary primary scope)))
                     (let ((ans2 (value-of-expression exp (answer-scope ans1))))
                       (cases eval-list (answer-val ans1)
                         (an-eval-list (py-list sc)
                                       (an-answer (answer-val (value-of-expression (list-ref py-list (answer-val ans2)) sc)) '- scope))))))
      (zero-arg-func-call (primary)
                          (let ((ans (value-of-primary primary scope)))
                            (an-answer (answer-val (apply-function (answer-val ans) (list) (answer-scope ans))) '- (answer-scope ans))))
      (args-func-call (primary args)
                          (let ((ans (value-of-primary primary scope)))
                            (an-answer (answer-val (apply-function (answer-val ans) args scope)) '- (answer-scope ans)))))))

(define value-of-atom
  (lambda (atom scope)
    (cond
      ((symbol? atom)
       (let ((scope-val (apply-scope scope atom)))
         (if (thunk? scope-val)
             (let ((exp-val (value-of-thunk scope-val)))
               (an-answer exp-val '- (extend-scope scope atom exp-val)))
             (an-answer scope-val '- scope))))
       ((py-list? atom) (an-answer (an-eval-list atom (copy-of-scope scope)) '- scope))
       (#t (an-answer atom '- scope)))))

(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp scope)
               (answer-val (value-of-expression exp scope))))))

(define value-of-param-with-default
  (lambda (pwd scope)
    (cases param-with-default pwd
      (a-param-with-default (ID-lhs exp)
                            (let ((exp-val (answer-val (value-of-expression exp scope)))
                                  (ID (value-of-assignment-lhs ID-lhs scope)))
                              (an-answer exp-val '- (extend-scope scope ID exp-val)))))))

(define value-of-print
  (lambda (pr scope)
    (cases print pr
      (print-exp (items)
                   (letrec ((eval-and-print-atoms (lambda (items scope res-list)
                                                    (if (null? items)
                                                        (begin
                                                          (cond
                                                           ((and (list? res-list) (null? (cdr res-list))) (displayln (car res-list)))
                                                           (else (displayln res-list)))
                                                          (an-answer (a-none) '- scope))
                                                        (let ((ans (value-of-atom (car items) scope)))
                                                          (eval-and-print-atoms (cdr items) (answer-scope ans) (append res-list (list (atom->printable (answer-val ans))))))))))
                     (eval-and-print-atoms items scope (list)))))))

(define atom->printable
  (lambda (at)
    (cond
      ((eval-list? at)
        (cases eval-list at
          (an-eval-list (py-list sc)
                        (map (lambda (a) (atom->printable
                                          (answer-val (value-of-expression a sc)))) py-list))))
      ((boolean? at)
       (if at 'True 'False))
      ((none? at) 'None)
      ((and (list? at) (null? (cdr at))) (car at))
      (else at))))

(define apply-function
  (lambda (func arg-list outer-scope)
    (cases function func
      (a-function (ID params statements scope)
                  (let ((scope (extend-scope scope ID func)))
                    (let ((scope (add-params-to-scope params scope)))
                      (let ((thunk-scope (copy-of-scope outer-scope)))
                        (let ((scope (add-args-to-scope arg-list params scope thunk-scope)))
                          (value-of-statements statements scope)))))))))

(define add-params-to-scope
  (lambda (params scope)
    (if (null? params)
        scope
        (let ((ans (value-of-param-with-default (car params) scope)))
          (add-params-to-scope (cdr params) (answer-scope ans))))))