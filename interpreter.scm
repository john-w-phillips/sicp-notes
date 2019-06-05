(define language-apply apply)

(define (let? expr) (eq? (car expr) 'let))

(define (named-let? expr) (symbol? (cadr expr)))

(define (let-bindings expr) 
  (if (named-let? expr)
      (caddr expr)
      (cadr expr)))

(define (let-seq expr) 
  (if (named-let? expr)
      (cdddr expr)
      (cddr expr)))

(define (let-name expr) (cadr expr))

(define (make-sequence values)
  (cons 'begin values))

(define (make-definition name value)
  (list 'define name value))
(define (make-application proc arglist)
  (cons proc arglist))
(define (make-function-definition name params value)
  (list 'define (cons name params) value))

(define (named-let->combination expr)
  (make-sequence
   (list
    (make-function-definition
     (let-name expr)
     (map car (let-bindings expr))
     (let-seq expr))
    (make-application
     (let-name expr)
     (map cadr (let-bindings expr))))))

(define (let->combination expr)
  (if (named-let? expr)
      (named-let->combination expr)
      (make-application
       (make-lambda
	(map car (let-bindings expr))
	(let-seq expr))
       (map cadr (let-bindings expr)))))

(define (eval-let expr env)
  (eval (let->combination expr) env))

(define (empty-bindings? bindings) (null? bindings))
(define (let*-bindings let*) (cadr let*))
(define (let*-body let*) (caddr let*))
(define first-binding car)
(define rest-bindings cdr)

(define (make-let bindings body)
  (list 'let bindings body))

(define (cons-binding binding rest)
  (cons binding rest))
(define (empty-bindings) '())

(define (let*-bindings->nested-lets let-body bindings)
  (if (empty-bindings? bindings)
      let-body
      (make-let
       (cons-binding (first-binding bindings) (empty-bindings))
       (let*-bindings->nested-lets let-body (rest-bindings bindings)))))

(define (let*->nested-lets expr)
  (let ((bindings (let*-bindings expr))
	(body (let*-body expr)))
    (let*-bindings->nested-lets body bindings)))

(define (eval-let* expr env)
  (eval (let*->nested-lets expr) env))

(define (application? exp)
  (and (pair? exp)
       (or (symbol? (car exp))
	   (application? (car exp)))))

(define (cond=>clause? clause)
  (eq? '=> (cadr clause)))

(define (expand-action cond-clause rest-clauses)
  (if (cond=>clause? cond-clause)
      (make-application
       (make-lambda '(expr-result)
		    (make-expr-sequence
		     (make-if
		      'expr-result
		      (make-application
		       (car (cond-actions cond-clause))
		       '(expr-result))
		      (expand-clauses rest-clauses))))
       (list (cond-predicate cond-clause)))
      (make-if
       (cond-predicate cond-clause)
       (sequence->exp (cond-actions cond-clause))
       (expand-clauses rest-clauses))))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last: COND->IF"
		       clauses))
	    (expand-action
	     first
	     rest)))))

(define (eval-and expr ops)
  (let ((first-op-value (eval (first-operand ops) env)))
    (if (false? first-op-value)
	first-op-value
	(eval-and-expr expr (rest-operands ops)))))
(define (true? value)
  (or (eq? value #t)
      (eq? value 'true)))

(define (eval-or expr ops)
  (let ((first-op-value (eval (first-operand ops) env)))
    (if (true? first-op-value)
	first-op-value
	(eval-and-expr expr (rest-operands ops)))))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
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
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))
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

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (first-operand exp)
  (car exp))

(define (rest-operands exp)
  (cdr exp))

(define (no-operands? exp)
  (null? exp))

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
	((let? exp) (eval (let->combination exp) env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else 
	 (error "Unknown expression type: EVAL " exp))))

(define (remove-bindings-from-frame var frame)
  (filter (lambda (binding) (not (eq? (car binding) var)))
	  frame))
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (let ((lookup (find-cell-in-frame var frame)))
      (if (null? lookup)
	  (make-unbound! var (enclosing-environment env))
	  (set-car! env (remove-bindings-from-frame var frame))))))

(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (let ((last-cell (last-pair frame)))
    (set-cdr! last-cell (list (cons var val)))))
;; extend-environment is the same.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define the-empty-environment '()) 
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (find-cell-in-frame var frame)
  (filter (lambda (x) (eq? (car x) var)) frame))

(define (set-cell-val! val cell)
  (set-cdr! (car cell) val))

(define (cell-val cell)
  (cdar cell))

(define (null-cell? cell) (null? cell))

(define (operate-on-cell proc var env)
  (if (eq? env the-empty-environment)
      (error "Undefined variable -- OPERATE-ON-CELL" var)
      (let ((lookup (find-cell-in-frame var (first-frame env))))
	(if (null-cell? lookup)
	    (operate-on-cell proc var (enclosing-environment env))
	    (proc lookup)))))

(define (lookup-variable-value var env)
  (operate-on-cell
   (lambda (cell)
     (cell-val cell))
   var env))

(define (set-variable-value! var val env)
  (operate-on-cell
   (lambda (cell)
     (set-cell-val! val cell))
   var env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((lookup (find-cell-in-frame var frame)))
	  (if (null? lookup)
	      (add-binding-to-frame! var val frame) 
	      (set-cdr! (car lookup) val)))))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (apply-primitive-procedure procedure arguments)
  (language-apply (cadr procedure) arguments))
(define (make-primitive-procedure proc)
  (list 'primitive-procedure proc))

(define (primitive-procedure? p)
  (eq? (car p) 'primitive-procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define primitive-environment
  (extend-environment
   (list
    '+
    '*
    'car 
    'cons 
    'cdr
    'null?)
   (list
    (make-primitive-procedure +)
    (make-primitive-procedure *)
    (make-primitive-procedure car)
    (make-primitive-procedure cons)
    (make-primitive-procedure cdr)
    (make-primitive-procedure null?))
   the-empty-environment))

(define (setup-environment)
  (let ((initial-env primitive-environment))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(driver-loop)
	  
