(define language-apply apply)

(load "syntax.scm")
(load "eval-apply.scm")



(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
;; (define (force-it obj)
;;   (if (thunk? obj)
;;       (actual-value (thunk-exp obj) (thunk-env obj))
;;       obj))
(define (force-it obj)
  (cond ((thunk? obj)
	 (let ((result
		(actual-value (thunk-exp obj)
			      (thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result)
	   (set-cdr! (cdr obj) '()) ;; forget unneeded env.
	   result))
	((evaluated-thunk? obj) (thunk-value obj))
	(else obj)))


(define (actual-value exp env)
  (force-it (eval exp env)))

(define (lazycons x y)
  (list 'cons-cell (lambda (p) (p x y))))
(define (lazycons-cell? expr)
  (tagged-list? expr 'cons-cell))
(define (lazycar c)
  ((cadr (force-it c)) (lambda (x y) x)))

(define (lazycdr c)
  ((cadr (force-it c)) (lambda (x y) y)))

(define (schemelist->lazylist in)
  (cond
   ((null? in)
    '())
   (else
    (lazycons (car in)
	      (schemelist->lazylist (cdr in))))))

(define (lazylist->schemelist in n)
  (cond
   ((= n 0) (list '...))
   ((null? (force-it in))
    '())
   ((not (lazycons-cell? (force-it in)))
    (list '. (force-it in)))
   ((equal? in (lazycdr in))
    (list (force-it (lazycar in)) '<infinite-repititions>))
   (else
    (cons (force-it (lazycar in))
	  (lazylist->schemelist (lazycdr in) (- n 1))))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (apply procedure arguments env)
  (cond
   ((primitive-procedure? procedure)
    (apply-primitive-procedure
     procedure
     (list-of-arg-values arguments env))) ;; changed
   ((lazy-primitive-procedure? procedure)
    (apply-primitive-procedure
     procedure
     (list-of-delayed-args arguments env)))
   ((compound-procedure? procedure)
    (eval-sequence
     (procedure-body procedure)
     (extend-environment
      (procedure-parameters procedure)
      (list-of-delayed-args arguments env) ;; changed
      (procedure-environment procedure))))
   (else (error "Unknown procedure type: APPLY"
		procedure))))
(define (interpret-text quoted-text)
  (cond
   ((pair? quoted-text)
    (schemelist->lazylist quoted-text))
   (else
    quoted-text)))

(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ((quoted? exp) (interpret-text (text-of-quotation exp)))
   ((assignment? exp) (eval-assignment exp env))
   ((definition? exp) (eval-definition exp env))
   ((if? exp) (eval-if exp env))
   ((lambda? exp) (make-procedure (lambda-parameters exp)
				  (lambda-body exp)
				  env))
   ((cond? exp) (eval (cond->if exp) env))
   ((let? exp) (eval (let->combination exp) env))
   ((begin? exp)
    (eval-sequence (begin-actions exp) env))
   ((application? exp)
    (apply (actual-value (operator exp) env)
	   (operands exp)
	   env))
   (else
    (error "Cannot evaluate expression -- EVAL" exp))))


(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
			  env)
	    (list-of-arg-values (rest-operands exps)
				env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
		      env)
	    (list-of-delayed-args (rest-operands exps)
				  env))))
