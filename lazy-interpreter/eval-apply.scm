(load "syntax.scm")
(load "environment.scm")
(load "primitive-apply.scm")

(define (eval-let expr env)
  (eval (let->combination expr) env))

(define (eval-let* expr env)
  (eval (let*->nested-lets expr) env))

(define (eval-and expr env)
  (define (eval-and-ops ops)
    (if (null? ops)
	#t
	(let ((first-op-value (eval (first-operand ops) env)))
	  (if (false? first-op-value)
	      first-op-value
	      (eval-and-ops (rest-operands ops))))))
  (eval-and-ops (operands expr)))

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


