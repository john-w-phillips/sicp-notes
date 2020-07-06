(load "baselib.scm")
(define-struct c-file-object (current main procs vars))
(define-struct c-statements (variables-need
			     variables-mod
			     statements))

;; All C procedures take argl and env as arguments.
(define-struct c-procedure (name statements))

(define empty-c-statements (make-c-statements '() '() '()))

(define (sort-module statements)
  (define (sort-iter output statements)
    (if (c-procedure? (car statements))
	(sort-iter (append-statement-sequences
	
(define (list-union l1 l2)
  (cond
   ((null? l1) l2)
   ((memq (car l1) l2)
    (list-union (cdr l1) l2))
   (else
    (cons (car l1)
	  (list-union (cdr l1) l2)))))
  
(define (list-diff l1 l2)
  (cond
   ((null? l1) '())
   ((memq (car l1) l2)
    (list-diff (cdr l1) l2))
   (else
    (cons (car l1) (list-diff (cdr l1) l2)))))

(define (append-statement-sequences . statements)
  (cond
   ((null? statements) empty-c-statements)
   (else
    (let ((rest-stmts (apply append-statement-sequences (cdr statements))))
      (make-c-statements
       (list-union
	(c-statements-variables-need (car statements))
	(list-diff  (c-statements-variables-need rest-stmts)
		    (c-statements-variables-mod (car statements))))
       (list-union 
	(c-statements-variables-mod rest-stmts)
	(c-statements-variables-mod (car statements)))
       (append (c-statements-statements  (car statements))
	       (c-statements-statements rest-stmts)))))))

(define (stringify x)
  (if (string? x)
      x
      (symbol->string x)))
;; Preserving regs for seq1, emit seq1.  We could make a temporary
;; global stack for this, called the 'savestack, rather than
;; 'formstack'.  alternatively we could also just use the formstack.
;; This may make sense because we could use preserving to run the garbage
;; collector.
(define (preserving vars seq1 seq2)
  (define (push-needed preserved modded saves pops actually-preserved)
    (if (null? preserved) (list saves pops actually-preserved)
	(if (and
	     (memq (car preserved) modded)
	     (memq (car preserved) (c-statements-variables-need seq1)))
	    (push-needed
	     (cdr preserved)
	     modded
	     (cons (strings-concat "PUSH (formstack, " (stringify (car preserved)) ")")
		   saves)
	     (cons (strings-concat
		    (stringify (car preserved))
		    " = POP (formstack, " (stringify (car preserved)) ")")
		   pops)
	     (cons (car preserved) actually-preserved))
	    (push-needed (cdr preserved)
			 modded
			 saves
			 pops
			 actually-preserved))))
  (let ((saves-pops
	 (push-needed vars (c-statements-variables-mod seq2)
		      '()
		      '()
		      '())))
    (append-statement-sequences
     (make-c-statements (caddr saves-pops) '() (car saves-pops))
     seq1
     (make-c-statements '() '() (reverse (cadr saves-pops)))
     seq2)))


;; (define c-standard-boilerplate (list "#include \"scheme.h\""))
(define (compile-self-evaluating expr target)
  (cond
   ((number? expr)
    (make-c-statements
     '()
     (list target)
     (list
      (strings-concat (stringify target) " = make_number (" (number->string expr) ")"))))
   ((string? expr)
    (make-c-statements
     '() (list target)
     (list
      (strings-concat (stringify target) " = make_string (\"" expr "\", false)"))))))

(define (compile-var-lookup expr target)
  (make-c-statements
   '(env)
   (list target)
   (list (strings-concat (stringify target)
			 " = env_find_var (env, make_symbol (\"" (stringify expr) "\", false))"))))

(define (compile-application expr target)
  (let ((proc-code (compile-to-c (car expr) 'proc))
	(args-code (compile-arglist-eval (cdr expr) 'argl))
	(apply-code (make-c-statements
		     '(proc env) (list target)
		     (list (strings-concat
			    (stringify target)
			    " = eval_apply (proc, argl, env)")))))
				       
    (preserving '(proc)
		proc-code
		(preserving '(env)
			    args-code
			    apply-code))))

(define (compile-arglist-eval argl target)
  (let ((arg-compilations (map (lambda (arg) (compile-to-c arg 'rval))
			       argl)))
    (define (compile-all-together args)
      (if (null? args) (make-c-statements
			'()
			(list target)
			(list (strings-concat (stringify target) " = NIL_VALUE")))
	  (append-statement-sequences
	   (compile-all-together (cdr args))
	   (car args)
	   (make-c-statements '(rval argl) (list target)
			      (list (strings-concat
				     (stringify target)
				     " = make_cons (rval," (stringify target) ")"))))))
    (compile-all-together arg-compilations)))
	  

;; (define (compile-application expr target c-file-object)
;;   (let ((proc-code (compile-to-c (car expr) 'proc c-file-object))
;; 	(arg-code (compile-argslist-to-c (cdr expr) 'argl c-file-object)))
;;     (append-statement-sequences
;;     (c-file-emit-statement proc-code)
;;     (c-file-emit-statement arg-code)
;;     (c-file-emit-statement (strings-concat target " = eval_apply (proc, argl, environ)"))))

(define (self-evaluating? expr)
  (or (number? expr) (string? expr)))

(define (variable? expr)
  (symbol? expr))

(define (application? expr)
  (pair? expr))

(define (compile-to-c expr target)
  (cond
   ((self-evaluating? expr)
    (compile-self-evaluating expr target))
   ((variable? expr)
    (compile-var-lookup expr target))
   ((application? expr)
    (compile-application expr target))
   (else (error "Unknown expression type"))))

(define (emit-to-list statements)
  (let ((vars-dcls
	 (map (lambda (var)
		(strings-concat "struct lisp_type * " (stringify var) " = NULL"))
	      (list-diff
	       (list-union (c-statements-variables-need statements)
			   (c-statements-variables-mod statements))
	       '(env)))))
    (append vars-dcls
	    (c-statements-statements statements))))
(define (emit-to-list-procedure procname statements)
  (append
   (list (strings-concat
	  "struct lisp_type *\n"
	  (stringify procname) " (struct lisp_type *env)\n{"))
   (append
    (emit-to-list statements)
    (list "}"))))

(define (needs-colon? a-string)
  (if (vec-contains? a-string ?{)
       false
       (not (vec-contains? a-string ?}))))

(define (write-to-file fname statement-list)
  (let ((file (open-port fname "w")))
    (define (write-iter statements)
      (if (null? statements) (close-port file)
	  (begin
	    (write-to-port file (car statements))
	    (if (needs-colon? (car statements))
		(write-to-port file ";\n")
		(write-to-port file "\n"))
	    (write-iter (cdr statements)))))
    (write-iter statement-list)))

;; (compile-to-c 1 'rval (make-default-c-file-object "hello.c"))
