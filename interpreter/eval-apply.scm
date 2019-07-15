(load "syntax.scm")
(load "environment.scm")
(load "primitive-apply.scm")

(define (eval-let expr env)
  (eval (let->combination expr) env))

(define (eval-let* expr env)
  (eval (let*->nested-lets expr) env))

(define (eval-and expr ops)
  (let ((first-op-value (eval (first-operand ops) env)))
    (if (false? first-op-value)
	first-op-value
	(eval-and-expr expr (rest-operands ops)))))

(define (eval-or expr ops)
  (let ((first-op-value (eval (first-operand ops) env)))
    (if (true? first-op-value)
	first-op-value
	(eval-and-expr expr (rest-operands ops)))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
	 (eval (first-exp exps) env))
	(else
	 (eval (first-exp exps) env)
	 (eval-sequence (rest-exps exps) env))))



(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)


;; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-value (list-of-values (rest-operands exps) env)))
	(let ((arg-value (eval (first-operand exps) env)))
 	  (cons arg-value
		rest-value)))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type: APPLY" procedure))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((letrec? exp) (eval (letrec->let exp) env))
	((let? exp) (eval (let->combination exp) env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else 
	 (error "Unknown expression type: EVAL " exp))))
