(load "regsim.scm")

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

(define (empty-bindings? bindings) (null? bindings))
(define (let*-bindings let*) (cadr let*))
(define (let*-body let*) (caddr let*))
(define first-binding car)
(define rest-bindings cdr)
(define (make-binding var val)
  (list var val))
(define (binding-var binding)
  (car binding))
(define (binding-val binding)
  (cadr binding))


(define (make-let bindings body)
  (cons 'let (cons bindings body)))

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
(define (empty-exps? seq) (null? seq))
(define (concat-exps seq1 seq2) (append seq1 seq2))
(define (sequence? exps)
  (and (pair? exps) (pair? (car exps))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (sequence-first sequence)
  (car sequence))

(define (sequence-rest sequence)
  (cdr sequence))

(define (empty-sequence? sequence)
  (null? sequence))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (legitimate-if? exp)
  (and (if? exp)
       (> (length exp) 2)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp)
  (if (not (legitimate-if? exp))
      (make-error "Bad IF statement")
      (cadr exp)))

(define (if-consequent exp)
  (if (not (legitimate-if? exp))
      (make-error "Bad IF statement")
      (caddr exp)))

(define (if-alternative exp)
  (if (not (legitimate-if? exp))
      (make-error "Bad IF statement")
      (if (not (null? (cdddr exp)))
	  (cadddr exp)
	  'false)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (if (not (legitimate-lambda? exp))
      (make-error "Malformed lambda")
      (cadr exp)))
(define (legitimate-lambda? exp)
  (and (lambda? exp)
       (> (length exp) 2)))

(define (lambda-body exp)
  (if (not (legitimate-lambda? exp))
      (make-error "Malformed lambda")
      (cddr exp)))

(define (definition? exp) (tagged-list? exp 'define))
(define (legitimate-definition? exp)
  (and (definition? exp)
       (> (length exp) 2)))

(define (definition-variable exp)
  (if (not (legitimate-definition? exp))
      (make-error "Bad definition")
      (if (symbol? (cadr exp))
	  (cadr exp)
	  (caadr exp))))

(define (definition-value exp)
  (if (not (legitimate-definition? exp))
      (make-error "Bad definition")
      (if (symbol? (cadr exp))
	  (caddr exp)
	  (make-lambda (cdadr exp)
		       (cddr exp)))))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (legitimate-assignment? exp)
  (and (assignment? exp)
       (= (length exp) 3)
       (symbol? (cadr exp))))

(define (assignment-variable exp)
  (if (not (legitimate-assignment? exp))
      (make-error "Bad assignment")
      (cadr exp)))

(define (assignment-value exp)
  (if (not (legitimate-assignment? exp))
      (make-error "Bad assignment")
      (caddr exp)))

(define (make-assignment var val) (list 'set! var val))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (make-quoted exp) (list 'quote exp))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	((boolean? exp) true)
	(else false)))



(define (primitive-procedure? p)
  (eq? (car p) 'primitive-procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (make-procedure parameters body env)
  (list 'procedure parameters (process-body body) env))

(define (process-body procedure-body) procedure-body)
;; (define (process-body procedure-body)
;;   (if (sequence? procedure-body)
;;       (scan-out-defines procedure-body)
;;       procedure-body))

;; (define (scan-out-defines procedure-body)
;;   (define (iterate-body body defines nondefines)
;;     (cond
;;      ((null? body)
;;       (list (reverse defines) (reverse nondefines)))
;;      ((definition? (first-exp body))
;;       (iterate-body
;;        (rest-exps body)
;;        (cons (first-exp body) defines)
;;        nondefines))
;;      (else
;;       (iterate-body
;;        (rest-exps body)
;;        defines
;;        (cons (first-exp body) nondefines)))))
;;   (define (create-let defines nondefines)
;;     (let ((binding-names (map definition-variable defines))
;; 	  (binding-vals (map definition-value defines)))
;;       (let ((bindings (map (lambda (name) (make-binding name (make-quoted UNDEFINED-VARIABLE)))
;; 			   binding-names))
;; 	    (sets (map (lambda (name val) (make-assignment name val))
;; 		       binding-names
;; 		       binding-vals)))
;; 	(list (make-let
;; 	       bindings
;; 		(concat-exps
;; 		 sets
;; 		 nondefines))))))
;;   (let ((body-iteration (iterate-body procedure-body '() '())))
;;     (let ((defines (car body-iteration))
;; 	  (nondefines (cadr body-iteration)))
;;       (if (null? defines)
;; 	  procedure-body
;; 	  (create-let defines nondefines)))))

(define (letrec? expr)
  (eq? (car expr) 'letrec))

(define (letrec-bindings expr)
  (cadr expr))

(define (letrec-body expr)
  (cddr expr))

(define (letrec->let expr)
  (let ((binding-vars (map binding-var (letrec-bindings expr))))
    (let ((undefined-bindings
	   (map (lambda (name) (make-binding
				name
				(make-quoted UNDEFINED-VARIABLE)))
		binding-vars))
	  (assignments
	   (map (lambda (binding)
		  (make-assignment
		   (binding-var binding)
		   (binding-val binding)))
		(letrec-bindings expr)))) 
      (make-let
       undefined-bindings
       (concat-exps
	assignments
	(letrec-body expr))))))

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

(define (and? expr) (tagged-list? expr 'and))
(define (or? expr) (tagged-list? expr 'or))
