#lang racket

(require eopl)
(provide (all-defined-out))

;environment datatype ------------------------------------------------------------------------------
(define environment? (list-of pair?))

(define empty-env (lambda () (list)))

(define apply-env
  (lambda (env var)
    (let ((found (assoc var env)))
      (if found
          (cdr found)
          (eopl:error 'apply-env "no value bounded for variable ~s" var)))))

(define extend-env
  (lambda (env var val)
    (cons (cons var val) env)))

(define globe (empty-env))

;scope datatype ------------------------------------------------------------------------------
(define-datatype scope scope?
  (global-scope)
  (local-scope
    (global-var-list (list-of symbol?))
    (env environment?)))

(define new-global-scope (lambda () (global-scope)))
(define new-local-scope (lambda (sc) (cases scope sc
                                       (global-scope () (local-scope (list) (empty-env)))
                                       (local-scope (gvl env) (local-scope (list) env)))))

(define apply-scope
  (lambda (sc var)
    (cases scope sc
      (global-scope () (apply-env globe var))
      (local-scope (global-var-list env)
        (cond
          ((member var global-var-list) (apply-env globe var))
          ((assoc var env) (apply-env env var))
          (else (eopl:error 'apply-scope "no value bounded for variable ~s" var)))))))

(define extend-scope
  (lambda (sc var val)
    (cases scope sc
      (global-scope () (begin
                         (set! globe (extend-env globe var val))
                         sc))
      (local-scope (global-var-list env)
                   (cond
                     ((member var global-var-list) (begin
                                                     (set! globe (extend-env globe var val))
                                                     sc))
                     (else (local-scope global-var-list (extend-env env var val))))))))

(define add-to-global-var-list
  (lambda (sc var)
    (cases scope sc
      (global-scope () sc)
      (local-scope (global-var-list env)
                   (local-scope (cons var global-var-list) env)))))

(define copy-of-scope
  (lambda (sc)
    (cases scope sc
      (global-scope () (local-scope '() globe))
      (local-scope (global-var-list env)
                   (local-scope '() (append (add-global-vars-to-env global-var-list env) env))))))

(define add-global-vars-to-env
  (lambda (global-var-list env)
    (if (null? global-var-list)
        env
        (add-global-vars-to-env (cdr global-var-list) (extend-env env (car global-var-list) (apply-env globe (car global-var-list)))))))
  
 
