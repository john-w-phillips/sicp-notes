(load "syntax.scm")
(load "environment.scm")
(load "primitive-apply.scm")

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
	((and? exp) (analyze-and exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((if-fail? exp) (analyze-if-fail exp))
	((let? exp) (analyze (let->combination exp)))
	((permanent-set? exp) (analyze-permanent-set exp))
;;	((require? exp) (analyze-require exp))
	((amb? exp) (analyze-amb exp))
	((ramb? exp) (analyze-ramb exp))
	((application? exp) (analyze-application exp))
	(else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (not (true? pred-value))
		   (fail2)
		   (succeed 'ok fail2)))
	     fail))))


(define (analyze-permanent-set exp)
  (let ((exp-var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc
       env
       (lambda (value fail2)
	 (set-variable-value! exp-var value env)
	 (succeed 'ok fail2))
       fail))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) (succeed exp fail)))

(define (analyze-and exp)
  (let ((exprs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (define (and-iter exprs)
	(cond
	 ((null? exprs) #t)
	 (else
	  (let ((first ((car exprs) env)))
	    (if (false? first)
		first
		(and-iter (cdr exprs)))))))
      (and-iter exprs))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	    (fail)
	    ((car choices)
	     env
	     succeed
	     (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;; This doesn't preserve order.
;; We don't need it to though.
(define (list-before lst index)
  (define (iter result lst index)
    (if (= index 0)
	result
	(iter (cons (car lst) result)
	      (cdr lst)
	      (- index 1))))
  (iter '() lst index))

(define (list-after lst index)
  (define (iter lst index)
    (if (= index 0)
	lst
	(iter (cdr lst) (- index 1))))
  (iter lst index))
	   

(define (get-random-choice listof-choices)
  (let* ((idx (random (length listof-choices)))
	 (before (list-before listof-choices idx))
	 (after (list-after listof-choices idx)))
    (cons (car after)
	  (append before (cdr after)))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	    (fail)
	    (let* ((chosen-pair (get-random-choice choices))
		   (chosen (car chosen-pair))
		   (others (cdr chosen-pair)))
	      (chosen
	       env
	       succeed
	       (lambda () (try-next others))))))
      (try-next cprocs))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
	     ;; successful continuation for evaluating
	     ;; the predicate.
	     (lambda (val fail2)
	       (if (truthy? val)
		   (cproc env succeed fail2)
		   (aproc env succeed fail2)))
	     fail))))


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-if-fail exp)
  (let ((tryexp (analyze (if-fail-try exp)))
	(failexp (analyze (if-fail-fail exp))))
    (lambda (env succeed fail)
      (tryexp
       env
       succeed
       (lambda () (failexp env succeed fail))))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc
       env
       (lambda (newval fail2)
	 (let ((old-value (lookup-variable-value var env)))
	   (set-variable-value! var newval env)
	   (succeed 'ok
		    (lambda ()
		      (set-variable-value! var old-value env)
		      (fail2)))))
       fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc
       env
       (lambda (procval fail2)
	 (define-variable! var procval env)
	 (succeed 'ok fail2))
       fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1
       env
       (lambda (val fail2)
	 (proc2 env succeed fail2))
       fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) 
	(error "Empty sequence: ANALYZE")
	(loop (car procs) (cdr procs)))))

 (define (get-args arglist env succeed fail)
      (if (null? arglist)
	  (succeed '() fail)
	  ((car arglist)
	   env
	   (lambda (argval fail2)
	     (get-args
	      (cdr arglist)
	      env
	      (lambda (vals fail3)
		(succeed (cons argval vals) fail3))
	      fail2))
	   fail)))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc
       env
       (lambda (fproc-value fail2)
	 (get-args
	  aprocs
	  env
	  (lambda (argslist fail3)
	    (execute-application
	     fproc-value
	     argslist
	     succeed
	     fail3))
	  fail2))
       fail))))


(define (eval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
	 (succeed (apply-primitive-procedure proc args)
		  fail))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment
	   (procedure-parameters proc)
	   args
	   (procedure-environment proc))
	  succeed
	  fail))
	(else
	 (error "Unknwon procedure type: EXECUTE-APPLICATION"
		proc)))) 
