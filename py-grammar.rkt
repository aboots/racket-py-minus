#lang eopl

(provide (all-defined-out))

;1. Program → Statements EOF
(define-datatype program program?
  (a-program (statements statements?))
  (checked-program
   (checked string?)
   (statements statements?)))

;2. Statements → Statement ‘; ‘ | Statements Statement ‘; ‘
(define-datatype statements statements?
  (single-statements
   (statement statement?))
  (mult-statements
   (statements statements?)
   (statement statement?)))

;3. Statement → Compound_stmt | Simple_stmt | Print
(define-datatype statement statement?
  (a-compound-stmt
   (compound-stmt compound-stmt?))
  (a-simple-stmt
   (simple-stmt simple-stmt?))
  (a-print-stmt
   (print-stmt print?)))

;4. Simple_stmt → Assignment | Return_stmt | Global_stmt
(define-datatype simple-stmt simple-stmt?
  (assignment-simple-stmt
   (assignment assignment?))
  (return-simple-stmt
   (return-stmt return-stmt?))
  (global-simple-stmt
   (global-stmt global-stmt?))
  (pass-stmt)
  (break-stmt)
  (continue-stmt))

;5. Compound_stmt → Function_def | If_stmt | For_stmt
(define-datatype compound-stmt compound-stmt?
  (func-def-comp-stmt
   (func-def function-def?))
  (if-comp-stmt
   (if-stmt if-stmt?))
  (for-comp-stmt
   (for-stmt for-stmt?)))

;6. Assignment → ID ‘ = ‘ Expression
(define-datatype assignment assignment?
  (an-assignment
   (ID symbol?)
   (exp expression?)))

;7. Return_stmt → ‘return‘ | ‘return‘ Expression
(define-datatype return-stmt return-stmt?
  (empty-return-stmt)
  (exp-return-stmt
   (exp expression?)))

;8. Global_stmt → ‘global‘ ID
(define-datatype global-stmt global-stmt?
  (a-global-stmt
   (ID symbol?)))

;9. Function_def → ‘def‘ ID ‘(‘ Params ‘)‘ ‘ : ‘ Statements|‘def‘ ID ‘() : ‘ Statements
(define-datatype function-def function-def?
  (params-func-def
   (ID symbol?)
   (params params?)
   (statements statements?))
  (zero-param-func-def
   (ID symbol?)
   (statements statements?)))

;10. Params → Param_with_default | Params ‘, ‘ Param_with_default
(define params? (lambda (e) (and (not (null? e)) (list-of param-with-default?))))

;11. Param_with_default → ID ‘ = ‘ Expression
(define-datatype param-with-default param-with-default?
  (a-param-with-default
   (ID symbol?)
   (exp expression?)))

;12. If_stmt → ‘if‘ Expression ‘ : ‘ Statements Else_block
(define-datatype if-stmt if-stmt?
  (an-if-stmt
   (exp expression?)
   (statements statements?)
   (else-block else-block?)))

;13. Else_block → ‘else‘ ‘ : ‘ Statements
(define-datatype else-block else-block?
  (an-else-block
   (statements statements?)))

;14. For_stmt → ‘for‘ ID ‘in‘ Expression ‘ : ‘ Statements
(define-datatype for-stmt for-stmt?
  (a-for-stmt
   (ID symbol?)
   (exp expression?)
   (statements statements?)))

;15. Expression → Disjunction
(define-datatype expression expression?
  (an-expression
   (disjunction disjunction?)))

;16. Disjunction → Conjunction | Disjunction ‘or‘ Conjunction
(define-datatype disjunction disjunction?
  (single-disjunction
   (conjunction conjunction?))
  (mult-disjunction
   (disjunction disjunction?)
   (conjunction conjunction?)))

;17. Conjunction → Inversion | Conjunction ‘and‘ Inversion
(define-datatype conjunction conjunction?
  (single-conjunction
   (inversion inversion?))
  (mult-conjunction
   (conjunction conjunction?)
   (inversion inversion?)))

;18. Inversion → ‘not‘ Inversion | Comparison
(define-datatype inversion inversion?
  (not-inversion
   (invresion inversion?))
  (a-comparison
   (comparison comparison?)))

;19. Comparison → eq-sum | lt-sum | gt-sum | sum
(define-datatype comparison comparison?
  (single-comparison (sum sum?))
  (eq-sum-comparison (eq-sum eq-sum?))
  (lt-sum-comparison (lt-sum lt-sum?))
  (gt-sum-comparison (gt-sum gt-sum?)))

;20. Eq_Sum → Sum ‘ == ‘ Sum
(define-datatype eq-sum eq-sum?
  (an-eq-sum
   (sum1 sum?)
   (sum2 sum?)))

;21. Lt_Sum → Sum ‘ < ‘ Sum
(define-datatype lt-sum lt-sum?
  (an-lt-sum
   (sum1 sum?)
   (sum2 sum?)))

;22. Gt_Sum → Sum ‘ > ‘ Sum
(define-datatype gt-sum gt-sum?
  (a-gt-sum
   (sum1 sum?)
   (sum2 sum?)))

;23. Sum → Sum ‘ + ‘ Term | Sum ‘ - ‘ Term | Term
(define-datatype sum sum?
  (add-sum
   (sum sum?)
   (term term?))
  (sub-sum
   (sum sum?)
   (term term?))
  (single-sum
   (term term?)))

;24. Term → Term ‘ ∗ ‘ Factor | Term ‘/‘ Factor | Factor
(define-datatype term term?
  (mul-term
   (term term?)
   (factor factor?))
  (div-term
   (term term?)
   (factor factor?))
  (single-term
   (factor factor?)))
   
;25. Factor → ‘ + ‘ Power | ‘ - ‘ Power | Power
(define-datatype factor factor?
  (pos-factor
   (power power?))
  (neg-factor
   (power power?))
  (single-factor
   (power power?)))
  
;26. Power → Atom ‘ ∗ ∗‘ Factor | Primary
(define-datatype power power?
  (a-pow
   (atom atom?)
   (factor factor?))
  (a-primary
   (primary primary?)))
   
;27. Primary → Atom | Primary ‘[‘ Expression ‘]‘ | Primary ‘()‘| Primary ‘(‘ Arguments ‘)‘
(define-datatype primary primary?
  (an-atom
   (atom atom?))
  (index-access
   (primary primary?)
   (exp expression?))
  (zero-arg-func-call
   (primary primary?))
  (args-func-call
   (primary primary?)
   (arguments arguments?)))

;28. Arguments → Expression | Arguments ‘, ‘ Expression
(define arguments? (lambda (e) (and (not (null? e)) (list-of expression?))))

;29. Atom → ID | ‘True‘ | ‘False‘ | ‘None‘ | NUMBER | List
(define atom?
  (lambda (a) (or (symbol? a) (boolean? a) (none? a) (number? a) (py-list? a))))

(define-datatype none none?
  (a-none))

;30. List → ‘[‘ Expressions ‘]‘ | ‘[]‘
(define py-list? (list-of expression?))

;31. Expressions → Expressions ‘, ‘ Expression | Expression
(define expressions? (lambda (e) (and (not (null? e)) (list-of expression?))))

;32. Print → ‘print (‘ Items ‘)‘
(define-datatype print print?
  (print-exp
   (items items?)))

;33. Items → Atom | Items‘,‘ Atom
(define items? (lambda (e) (or (atom? e) (list-of atom?))))
