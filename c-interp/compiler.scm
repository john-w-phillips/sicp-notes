(load "baselib.scm")
(load "files.scm")
(define-struct c-file-object (current main procs vars))
(define-struct c-statements (variables-need
			     variables-mod
			     statements))

;; One kind of statement is not a list but a struct representing a
;; separate function, which has a procedure name and inner statements.
(define-struct c-procedure-statement (statement-name inner-statements))

(define empty-c-statements (make-c-statements '() '() '()))

(define (wrap-module-statements need mod statements)
  (make-c-procedure-statement
   'init_mod
   (end-with-linkage
    'return
    (make-c-statements
     need
     mod
     statements))))

(define (prepare-sort-module statements)
  (let ((need (c-statements-variables-need statements))
	(mod (c-statements-variables-mod statements)))
  (define (sort-iter proc-output normal-output statements)
    (cond ((null? statements)
	   (append (reverse proc-output) (list (wrap-module-statements
						need mod
						(reverse normal-output)))))
	  ((c-procedure-statement? (car statements))
	   (sort-iter (cons (car statements) proc-output)
		      normal-output
		      (cdr statements)))
	  (else
	   (sort-iter proc-output
		      (cons (car statements) normal-output)
		      (cdr statements)))))
  (make-c-statements
   (c-statements-variables-need statements)
   (c-statements-variables-mod statements)
   (sort-iter '() '() (c-statements-statements statements)))))

  
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

(define (tack-on-statement-sequences . statements)
  (define (tack-2-statement-seqs s1 s2)
    (make-c-statements
     (list-union (c-statements-variables-need s1)
		 (c-statements-variables-need s2))
     (list-union (c-statements-variables-mod s1)
		 (c-statements-variables-mod s2))
     (append (c-statements-statements s1)
	     (c-statements-statements s2))))
  (define (tack-iter statements)
    (if (null? statements) empty-c-statements
	(tack-2-statement-seqs (car statements)
			       (tack-iter (cdr statements)))))
  (tack-iter statements))


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
	     (memq (car preserved) (c-statements-variables-need seq2)))
	    (push-needed
	     (cdr preserved)
	     modded
	     (cons (strings-concat "PUSH_STACK (formstack, " (stringify (car preserved)) ")")
		   saves)
	     (cons (strings-concat
		    (stringify (car preserved))
		    " = POP_STACK (formstack)")
		   pops)
	     (cons (car preserved) actually-preserved))
	    (push-needed (cdr preserved)
			 modded
			 saves
			 pops
			 actually-preserved))))
  (let ((saves-pops
	 (push-needed vars (c-statements-variables-mod seq1)
		      '()
		      '()
		      '())))
    (append-statement-sequences
     (make-c-statements (caddr saves-pops) '() (car saves-pops))
     seq1
     (make-c-statements '() '() (reverse (cadr saves-pops)))
     seq2)))


;; (define c-standard-boilerplate (list "#include \"scheme.h\""))
(define (compile-self-evaluating expr target linkage)
  (cond
   ((number? expr)
    (end-with-linkage
     linkage
     (make-c-statements
      '()
      (list target)
      (list
       (strings-concat (stringify target) " = make_number (" (number->string expr) ")")))))
   ((string? expr)
    (end-with-linkage
     linkage
     (make-c-statements
      '() (list target)
      (list
       (strings-concat (stringify target) " = make_string (\"" expr "\", false)")))))))

(define (compile-var-lookup expr target linkage)
  (end-with-linkage
   linkage
   (make-c-statements
    '(env)
    (list target)
    (list (strings-concat (stringify target)
			  " = env_find_var (env, make_symbol (\"" (stringify expr) "\", false))")
	  (strings-concat "if (" (stringify target) ") {")
	  (strings-concat (stringify target) " = cdr (" (stringify target) ")")
	  " } else {"
	  (strings-concat
	   "scheme_signal_eval_error (\"Undefined variable %s\", \""
	   (stringify expr) "\")")
	  "}"))))

(define (compile-application expr target linkage)
  (let ((proc-code (compile-to-c (car expr) 'proc 'next))
	(args-code (compile-arglist-eval (cdr expr) 'argl))
	(apply-code
	 (make-c-statements
	  '(proc env argl)
	  (list target)
	  (list
	    "PUSH_STACK (formstack, argl)"
	    "PUSH_STACK (formstack, env)"
	    "PUSH_STACK (formstack, proc)"
	    "gc (false)"
	    (strings-concat
	     (stringify target)
	     " = eval_apply (proc, argl, env)")
	    "POP_STACK (formstack)"
	    "POP_STACK (formstack)"
	    "POP_STACK (formstack)"))))
    (end-with-linkage
     linkage
     (append-statement-sequences
      proc-code
     (preserving '(proc)
		 args-code
		 apply-code)))))


(define (compile-arglist-eval argl target)
  (let ((arg-compilations (map (lambda (arg) (compile-to-c arg 'rval 'next))
			       argl)))
    (define (compile-all-together args)
      (if (null? args) (make-c-statements
			'()
			(list target)
			(list (strings-concat (stringify target) " = NIL_VALUE")))
	  (append-statement-sequences
	   (compile-all-together (cdr args))
	   (preserving
	    (list target)
	    (car args)
	    (make-c-statements '(rval argl) (list target)
			       (list (strings-concat
				      (stringify target)
				      " = make_cons (rval," (stringify target) ")")))))))
      (compile-all-together arg-compilations)))


(define (end-with-linkage linkage statements)
  (cond
   ((eq? linkage 'next) statements)
   ((eq? linkage 'return)
    (append-statement-sequences
     statements
     (make-c-statements '(rval) '() (list "return rval"))))
   (else (error "Unknown linkage!"))))
    
    
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
(define *lambda-num* 1)

(define (gen-anon-lambda-name)
  (let ((rval (strings-concat
	       "scheme_anon_func" (number->string *lambda-num*))))
    (set! *lambda-num* (+ *lambda-num* 1))
    rval))

(define (lambda? expr)
  (and (pair? expr) (eq? (car expr) 'lambda)))

(define (quoted? expr)
  (and (pair? expr) (eq? (car expr) 'quote)))

(define (lambda-body expr)
  (cddr expr))
	      
(define (lambda-formals expr)
  (cadr expr))

(define (compile-sequence-to-c exprs target linkage)
  (cond
   ((null? (cdr exprs))
     (compile-to-c (car exprs) target linkage))
   (else
    (append-statement-sequences
     (compile-to-c (car exprs) target 'next)
     (compile-sequence-to-c (cdr exprs) target linkage)))))

(define (compile-lambda expr target linkage)
  (let ((body (compile-sequence-to-c (lambda-body expr) 'rval 'return))
	(arglist (compile-quotation (lambda-formals expr) 'argl 'next))
	(anon-name (gen-anon-lambda-name)))
    (end-with-linkage
     linkage
     (append-statement-sequences
      arglist
      (make-c-statements
       '(env argl)
       (list target)
       (list
	(make-c-procedure-statement anon-name body)
	(strings-concat (stringify target)
			" = make_compiled_procedure ( "
			(stringify anon-name)
			", argl, env)")))))))

(define (if? expr) (and (pair? expr) (eq? (car expr) 'if)))
(define (if-predicate expr) (cadr expr))
(define (if-consequent expr) (caddr expr))
(define (if-alternative expr) (cadddr expr))

(define (compile-if expr target linkage)
  (let ((pred-code (compile-to-c (if-predicate expr) 'rval 'next))
	(consequent-code (compile-to-c (if-consequent expr) target 'next))
	(alternative-code (compile-to-c (if-alternative expr) target 'next)))
    (end-with-linkage linkage
		      (append-statement-sequences
		       pred-code
		       (tack-on-statement-sequences
			(make-c-statements
			 '(rval)
			 '()
			 (list "if (truep (rval)) \n{"))
			consequent-code
			(make-c-statements
			 '()
			 '()
			 (list "}\nelse\n{"))
			alternative-code
			(make-c-statements
			 '()
			 '()
			 (list "\n}\n")))))))

(define (compile-quotation expr target linkage)
  (cond
   ((self-evaluating? expr)
    (compile-self-evaluating expr target linkage))
   ((null? expr)
    (make-c-statements
     '() (list target)
     (list (strings-concat (stringify target) " = NIL_VALUE"))))
   ((symbol? expr)
    (make-c-statements
     '()
     (list target)
     (list 
     (strings-concat (stringify target)
		     " = make_symbol(\"" (stringify expr)
		     "\", false)"))))
   ((pair? expr)
    (let ((car-code (compile-quotation (car expr) 'rval linkage))
	  (cdr-code (compile-quotation (cdr expr) 'argl linkage)))
      (append-statement-sequences
       cdr-code
      (preserving
       '(argl)
       car-code
       (make-c-statements
	'(argl rval)
	(list target)
	(list (strings-concat (stringify target) " = make_cons (rval, argl)")))))))
   (else (error "Bad expression type for quotation"))))

(define (begin? expr)
  (eq? (car expr) 'begin))
(define (begin-exprs expr)
  (cdr expr))

(define (definition? expr)
  (and (pair? expr) (eq? (car expr) 'define)))

(define (definition-value expr)
  (caddr expr))

(define (definition-variable expr)
  (cadr expr))

(define (compile-definition expr target linkage)
  (let ((assigner-code (compile-to-c
			(definition-value expr)
			target
			'next))
	(var (definition-variable expr)))
    (end-with-linkage
     linkage
     (append-statement-sequences
      assigner-code
      (make-c-statements
       (list target 'env)
       '()
       (list
	(strings-concat 
	"env_frame_set_var_value (environ_first_frame (env), make_symbol (\""
	(stringify var) "\", false), rval)")))))))


(define (compile-to-c expr target linkage)
  (cond
   ((self-evaluating? expr)
    (compile-self-evaluating expr target linkage))
   ((variable? expr)
    (compile-var-lookup expr target linkage))
   ((quoted? expr)
    (compile-quotation (cadr expr) target linkage))
   ((if? expr)
    (compile-if expr target linkage))
   ((definition? expr)
    (compile-definition expr target linkage))
   ((lambda? expr)
    (compile-lambda expr target linkage))
   ((begin? expr)
    (compile-sequence-to-c (begin-exprs expr) target linkage))
   ((application? expr)
    (compile-application expr target linkage))
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

(define (make-decls stmts)
  (let ((decl-mods
	 (list-diff
	  (c-statements-variables-mod stmts)
	  (c-statements-variables-need stmts))))
    (map (lambda (x) (strings-concat "struct lisp_type * " (stringify x) " = NULL;\n"))
	 decl-mods)))

(define (make-params stmts)
  (let ((decl-needs (c-statements-variables-need stmts)))
    (define (params-iter pstring decl-vars)
      (cond ((null? decl-vars) " ()")
	    ((null? (cdr decl-vars))
	     (strings-concat pstring "struct lisp_type * " (stringify (car decl-vars)) ")"))
	    (else
	     (params-iter
	      (strings-concat pstring "struct lisp_type * " (stringify (car decl-vars)) ", ")
	      (cdr decl-vars)))))
    (params-iter " (" decl-needs)))


(define (write-procedure file statement)
  (let ((name (stringify (c-procedure-statement-statement-name statement)))
	(stmts (c-procedure-statement-inner-statements statement)))
    (let ((var-decls (make-decls stmts))
	  (var-params (make-params stmts)))
      (write-to-port file (strings-concat "struct lisp_type *\n"
					  (stringify name)))
      (write-to-port file var-params)
      (write-to-port file "\n{\n")
      (map (lambda (decl) (write-to-port file decl)) var-decls)
      (write-statements-iter file (c-statements-statements stmts))
      (write-to-port file "}\n"))))

(define (write-statements-iter file statements)
  (cond ((null? statements) true)
	((c-procedure-statement? (car statements))
	 (begin
	   (write-procedure file (car statements))
	   (write-statements-iter file (cdr statements))))
	(else
	 (begin
	   (write-to-port file (car statements))
	   (if (needs-colon? (car statements))
	       (write-to-port file ";\n")
	       (write-to-port file "\n"))
	   (write-statements-iter file (cdr statements))))))

(define (write-to-file fname statement-list)
  (let ((file (open-port fname "w")))
    (write-to-port file "#include \"scheme.h\"\n")
    (write-statements-iter file statement-list)
    (close-port file)))

(define (compile-as-module expr fname)
  (let ((statements (compile-to-c expr 'rval 'next)))
    (let ((module (prepare-sort-module statements)))
      (write-to-file fname (c-statements-statements module)))))

(define (compile-file fname)
  (let ((scheme-code (read-scheme-file fname)))
    (compile-as-module scheme-code fname)))
;; (compile-to-c 1 'rval (make-default-c-file-object "hello.c"))
