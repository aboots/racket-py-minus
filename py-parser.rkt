#lang racket

(require parser-tools/yacc)

(require "py-lexer.rkt"
         "py-grammar.rkt")

(provide (all-defined-out))

(define py-parser
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens token-type-a token-type-b)
   (grammar
    (program
     ((statements) (a-program $1))
     ((CHECKED statements) (checked-program $2)))
    (statements
     ((statement SEMICOL) (single-statements $1))
     ((statements statement SEMICOL) (mult-statements $1 $2)))
    (statement
     ((compound-stmt) (a-compound-stmt $1))
     ((simple-stmt) (a-simple-stmt $1))
     ((print) (a-print-stmt $1)))
    (simple-stmt
     ((assignment) (assignment-simple-stmt $1))
     ((global-stmt) (global-simple-stmt $1))
     ((return-stmt) (return-simple-stmt $1))
     ((PASS) (pass-stmt))
     ((BREAK) (break-stmt))
     ((CONTINUE) (continue-stmt)))
    (compound-stmt
     ((function-def) (func-def-comp-stmt $1))
     ((if-stmt) (if-comp-stmt $1))
     ((for-stmt) (for-comp-stmt $1)))
    (assignment
     ((assignment-lhs ASSIGN expression) (an-assignment $1 $3)))
    (return-stmt
     ((RETURN) (empty-return-stmt))
     ((RETURN expression) (exp-return-stmt $2)))
    (global-stmt
     ((GLOBAL ID) (a-global-stmt $2)))
    (function-def
     ((DEF ID OPEN-PAR params CLOSE-PAR return-type statements) (params-func-def $2 $4 $6 $7))
     ((DEF ID ZERO-ARG return-type statements) (zero-param-func-def $2 $4 $5)))
    (params
     ((param-with-default) (list $1))
     ((params COMMA param-with-default) (append $1 (list $3))))
    (param-with-default
     ((assignment-lhs ASSIGN expression) (a-param-with-default $1 $3)))
    (if-stmt
     ((IF expression COLON statements else-block) (an-if-stmt $2 $4 $5)))
    (else-block
     ((ELSE COLON statements) (an-else-block $3)))
    (for-stmt
     ((FOR ID IN expression COLON statements) (a-for-stmt $2 $4 $6)))
    (expression
     ((disjunction) (an-expression $1)))
    (disjunction
     ((conjuction) (single-disjunction $1))
     ((disjunction OR conjuction) (mult-disjunction $1 $3)))
    (conjuction
     ((inversion) (single-conjunction $1))
     ((conjuction AND inversion) (mult-conjunction $1 $3)))
    (inversion
     ((NOT inversion) (not-inversion $2))
     ((comparison) (a-comparison $1)))
    (comparison
     ((eq-sum) (eq-sum-comparison $1))
     ((lt-sum) (lt-sum-comparison $1))
     ((gt-sum) (gt-sum-comparison $1))
     ((sum) (single-comparison $1)))
    (eq-sum
     ((sum EQUAL sum) (an-eq-sum $1 $3)))
    (lt-sum
     ((sum LESS sum) (an-lt-sum $1 $3)))
    (gt-sum
     ((sum GREATER sum) (a-gt-sum $1 $3)))
    (sum
     ((sum PLUS term) (add-sum $1 $3))
     ((sum MINUS term) (sub-sum $1 $3))
     ((term) (single-sum $1)))
    (term
     ((term MUL factor) (mul-term $1 $3))
     ((term DIV factor) (div-term $1 $3))
     ((factor) (single-term $1)))
    (factor
     ((PLUS power) (pos-factor $2))
     ((MINUS power) (neg-factor $2))
     ((power) (single-factor $1)))
    (power
     ((atom POW factor) (a-pow $1 $3))
     ((primary) (a-primary $1)))
    (primary
     ((atom) (an-atom $1))
     ((primary OPEN-BRACKET expression CLOSE-BRACKET) (index-access $1 $3))
     ((primary ZERO-ARG) (zero-arg-func-call $1))
     ((primary OPEN-PAR arguments CLOSE-PAR) (args-func-call $1 $3)))
    (arguments
     ((expression) (list $1))
     ((arguments COMMA expression) (append $1 (list $3))))
    (atom
     ((ID) $1)
     ((TRUE) #t)
     ((FALSE) #f)
     ((NONE) (a-none))
     ((NUM) $1)
     ((list) $1))
    (list
     ((OPEN-BRACKET expressions CLOSE-BRACKET) $2)
     ((EMPTY-LIST) (list)))
    (expressions
     ((expressions COMMA expression) (append $1 (list $3)))
     ((expression) (list $1)))
    (print
     ((PRINT OPEN-PAR items CLOSE-PAR) (print-exp $3)))
    (items
     ((atom) (list $1))
     ((items COMMA atom) (append $1 (list $3))))
    (type
     ((INT) (int-type))
     ((FLOAT) (float-type))
     ((BOOL) (bool-type))
     ((LIST) (list-type))
     ((NONE) (none-type)))
    (assignment-lhs
     ((ID) (assign-without-type $1))
     ((ID COLON type) (assign-with-type $1 $3)))
    (return-type
     ((COLON) (return-no-type))
     ((RETURNSYMBOL type COLON) (return-type-func $2))))))

(define (lex-and-parse input) (a-inner-program (py-parser (lex input))))

;(lex-and-parse "c = 1 < 7 and 13 > 17 or 1 == 1;")