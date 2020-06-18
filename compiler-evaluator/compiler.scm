(load "eceval-syntax.scm")
;;(load "../eceval-impl/environment.scm")
(load "lexical-addresses.scm")
;;(load "primitive-apply.scm")
;;(load "assembler.scm")
;;(load "../eceval-impl/regsim.scm")
;; (load "../eceval-impl/eceval-ops.scm")
;; (load "../eceval-impl/syntax.scm")

;; (define (get-global-environment)
;;   the-global-environment)

;; (define the-global-environment (setup-environment))

(define (make-compiled-procedure entry-label env)
  (list 'compiled-procedure entry-label env))

(define (compiled-procedure? proc)
  (and (pair? proc) (eq? (car proc) 'compiled-procedure)))

(define (compiled-procedure-entry proc)
  (cadr proc))

(define (compiled-procedure-env proc)
  (caddr proc))

(define *label-num* 1)
(define all-regs '(val exp proc continue env))

(define (make-label name)
  (let ((labelstring (symbol->string name))
	(number (number->string *label-num*)))
    (let ((rval
	   (string->symbol
	    (string-append
	     labelstring
	     number))))
      (set! *label-num* (+ *label-num* 1))
      rval)))

(define (compile exp compenv target linkage)
  (cond ((self-evaluating? exp)
	 (compile-self-evaluating exp compenv target linkage))
	((quoted? exp) (compile-quoted exp compenv target linkage))
	((variable? exp) (compile-variable exp compenv target linkage))
	((assignment? exp)
	 (compile-assignment exp compenv target linkage))
	((definition? exp)
	 (compile-definition exp compenv target linkage))
	((if? exp) (compile-if exp compenv target linkage))
	((lambda? exp) (compile-lambda exp compenv target linkage))
	((let? exp) (compile (let->combination exp)
			     compenv
			     target
			     linkage))
	((begin? exp)
	 (compile-sequence
	  (begin-actions exp)
	  compenv
	  target linkage))
	((cond? exp)
	 (compile (cond->if exp) compenv target linkage))
	((open-coded? exp compenv)
	 (compile-open-coded exp compenv target linkage))
	((compiled-boolean? exp)
	 (compile-boolean-expr exp compenv target linkage))
	((application? exp)
	 (compile-application exp compenv target linkage))
	(else
	 (error "Unknown expression type: COMPILE" exp))))

(define (make-instruction-sequence
	 needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
	 (make-instruction-sequence
	  '(continue) '()
	  '((goto (reg continue)))))
	((eq? linkage 'next)
	 (empty-instruction-sequence))
	(else
	 (make-instruction-sequence '() '()
				    `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
	      instruction-sequence
	      (compile-linkage linkage)))

(define (compile-self-evaluating exp compenv target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '()
			      (list target)
			      `((assign ,target (const ,exp))))))

(define (compile-quoted exp compenv target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
			      `((assign ,target
					(const ,(text-of-quotation exp)))))))

(define (compile-variable exp compenv target linkage)
  (let ((find-result (find-variable exp compenv)))
    (if find-result
	(end-with-linkage
	 linkage
	 (make-instruction-sequence
	  '(env) (list target)
	  `((assign ,target
		    (op lexical-address-lookup)
		    (const ,find-result)
		    (reg env)))))
	
	(end-with-linkage
	 linkage
	 (make-instruction-sequence
	  '(env) (list target)
	  `((assign ,target
		    (op lookup-variable-value)
		    (const ,exp)
		    (reg env))))))))

(define (compile-assignment exp compenv target linkage)
  (let ((var (assignment-variable exp))
	(get-value-code
	 (compile (assignment-value exp) compenv 'val 'next)))
    (let ((lexical-address-of-var (find-variable var compenv)))
      (let ((assign-code
	     (if lexical-address-of-var
		 `((perform (op lexical-address-set!)
		    (const ,lexical-address-of-var)
		    (reg env)
		    (reg val))
		   (assign ,target (const ok)))
		 `((perform (op set-variable-value!)
			    (const ,var)
			    (reg val)
			    (reg env))))))
		 
      (end-with-linkage
       linkage
       (preserving
	'(env)
	get-value-code
	(make-instruction-sequence
	 '(env val) (list target)
	 assign-code)))))))

(define (compile-definition exp compenv target linkage)
  (let ((var (definition-variable exp))
	(get-value-code
	 (compile (definition-value exp) compenv 'val 'next)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op define-variable!)
		  (const ,var)
		  (reg val)
		  (reg env))
	 (assign ,target (const ok))))))))

(define (compile-if exp compenv target linkage)
  (let ((t-branch (make-label 'true-branch))
	(f-branch (make-label 'false-branch))
	(after-if (make-label 'after-if)))
    (let ((consequent-linkage
	   (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) compenv 'val 'next))
	    (c-code
	     (compile
	      (if-consequent exp) compenv target consequent-linkage))
	    (a-code (compile (if-alternative exp) compenv target linkage)))
	(preserving
	 '(env continue)
	 p-code
	 (append-instruction-sequences
	  (make-instruction-sequence
	   '(val) '()
	   `((test (op false?) (reg val))
	     (branch (label ,f-branch))))
	  (parallel-instruction-sequences
	   (append-instruction-sequences t-branch c-code)
	   (append-instruction-sequences f-branch a-code))
	  after-if))))))


(define (compile-bool testop-symb name)
  (lambda (exp compenv target linkage)
    (let ((return-label (make-label (string->symbol (string-append name "-return"))))
	  (codes (map (lambda (expr) (compile expr compenv target 'next))
		      (cdr exp))))
      (define (iter-ops op-codes)
	(if (null? op-codes)
	    (empty-instruction-sequence)
	    
	    (append-instruction-sequences
	     (car op-codes)
	     
	     (make-instruction-sequence
	      (list target)
	      '()
	      `((test (op ,testop-symb) (reg ,target))
		(branch (label ,return-label))))

	     (iter-ops (cdr op-codes)))))
      (end-with-linkage
       linkage
       (append-instruction-sequences
	(iter-ops codes)
	return-label)))))

(define boolean-table
  `((and ,(compile-bool 'false? "and"))
    (or ,(compile-bool 'true? "or"))))

(define (compiled-boolean? exp)
  (memq (car exp) (map car boolean-table)))
(define (compile-boolean-expr exp compile-env target linkage)
  (let ((proc-entry (assq (car exp) boolean-table)))
    ((cadr proc-entry) exp compile-env target linkage)))

(define (compile-lambda-body exp compenv proc-entry)
  (let ((formals (lambda-parameters exp)))
    (let ((ext-compenv (compile-time-env-extend formals compenv))
	  (new-lambda (transform-lambda exp)))
      (append-instruction-sequences
       (make-instruction-sequence
	'(env proc argl) '(env)
	`(,proc-entry
	  (assign env
		  (op compiled-procedure-env)
		  (reg proc))
	  (assign env
		  (op extend-environment)
		  (const ,formals)
		  (reg argl)
		  (reg env))))
       (compile-sequence (lambda-body new-lambda)
			 ext-compenv 'val 'return)))))

(define (scan-out-defines lambda-body)
  (define (scan-out-iter defines-names defines-as-list-of-lambdas body unparsed)
    (cond
     ((null? unparsed)
      (list defines-names defines-as-list-of-lambdas body))
     ((and (definition? (car unparsed))
	   (lambda? (definition-value (car unparsed))))
      (scan-out-iter
       (cons (definition-variable (car unparsed))
	     defines-names)
       (cons (definition-value (car unparsed))
	     defines-as-list-of-lambdas)
       body
       (cdr unparsed)))
     (else
      (scan-out-iter
       defines-names
       defines-as-list-of-lambdas
       (cons (car unparsed) body)
       (cdr unparsed)))))
  (scan-out-iter '() '() '() lambda-body))

(define scanned-out-params car)
(define scanned-out-bodies cadr)
(define scanned-out-sequence caddr)

(define (transform-lambda exp)
  (let ((body (lambda-body exp)))
    (let ((scanned-out (scan-out-defines body)))
      (if (null? (scanned-out-params scanned-out))
	  exp
	  `(lambda ,(lambda-parameters exp)
	     ((lambda ,(scanned-out-params scanned-out)
		,@(map (lambda (param lamb)
			 `(set! ,param ,lamb))
		       (scanned-out-params scanned-out)
		       (scanned-out-bodies scanned-out))
		,@(scanned-out-sequence scanned-out))
	      ,@(map (lambda (ignore) '(quote ()))
		     (scanned-out-params scanned-out))))))))

(define (compile-lambda exp compenv target linkage)
  (let ((proc-entry (make-label 'entry))
	(after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
	   (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
	(end-with-linkage
	 lambda-linkage
	 (make-instruction-sequence
	  '(env) (list target)
	  `((assign ,target
		    (op make-compiled-procedure)
		    (label ,proc-entry)
		    (reg env)))))
	(compile-lambda-body exp compenv proc-entry))
       after-lambda))))

(define (compile-application exp compenv target linkage)
  (let ((proc-code (compile (operator exp) compenv 'proc 'next))
	(operand-codes
	 (map (lambda (operand) (compile operand compenv 'val 'next))
	      (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))
  
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
	(make-instruction-sequence
	 '() '(argl)
	 '((assign argl (const ()))))
	(let ((code-to-get-last-arg
	       (append-instruction-sequences
		(car operand-codes)
		(make-instruction-sequence
		 '(val) '(argl)
		 '((assign argl (op list) (reg val)))))))
	  (if (null? (cdr operand-codes))
	      code-to-get-last-arg
	      (preserving
	       '(env)
	       code-to-get-last-arg
	       (code-to-get-rest-args
		(cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
	 (preserving
	  '(argl)
	  (car operand-codes)
	  (make-instruction-sequence
	   '(val argl) '(argl)
	   '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
	code-for-next-arg
	(preserving
	 '(env)
	 code-for-next-arg
	 (code-to-get-rest-args (cdr operand-codes))))))
		    
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(compound-branch (make-label 'compound-branch))
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage
	   (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
	'(proc) '()
	`((test (op primitive-procedure?) (reg proc))
	  (branch (label ,primitive-branch))
	  (test (op compound-procedure?) (reg proc))
	  (branch (label ,compound-branch))))

       (parallel-instruction-sequences

	(append-instruction-sequences
	 compiled-branch
	  (compile-proc-appl target compiled-linkage))

	(append-instruction-sequences
	 compound-branch
	 (end-with-linkage
	  compiled-linkage
	  (make-instruction-sequence
	   '(proc argl)
	   all-regs
	   `((goto (reg compapp))))))

	(append-instruction-sequences
	 primitive-branch
	 (end-with-linkage
	  linkage
	  (make-instruction-sequence
	   '(proc argl)
	   (list target)
	   `((assign ,target
		     (op apply-primitive-procedure)
		     (reg proc)
		     (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
	 (make-instruction-sequence
	  '(proc)
	  all-regs
	  `((assign continue (label ,linkage))
	    (assign val
		    (op compiled-procedure-entry)
		    (reg proc))
	    (goto (reg val)))))
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence
	    '(proc)
	    all-regs
	    `((assign continue (label ,proc-return))
	      (assign val (op compiled-procedure-entry)
		      (reg proc))
	      (goto (reg val))
	      ,proc-return
	      (assign ,target (reg val))
	      (goto (label ,linkage))))))
	((and (eq? target 'val) (eq? linkage 'return))
	 (make-instruction-sequence
	  '(proc continue)
	  all-regs
	  '((assign val (op compiled-procedure-entry)
		    (reg proc))
	    (goto (reg val)))))
	((and (not (eq? target 'val))
	      (eq? linkage 'return))
	 (error "return linkage, target not val: COMPILE"
		target))))



(define (rewrite-nary-as-binary expr)
  (let ((op (car expr)))
    (define (rewrite-inner operands)
      (cond
       ((= (length operands) 2)
	(cons op operands))
       ((> (length operands) 2)
	(list op (car operands) (rewrite-inner (cdr operands))))
       (else
	(error "Invalid n-ary expression: COMPILE" expr))))
    (rewrite-inner (cdr expr))))

(define (open-code-nary-proc procsym)
  (let ((binary-proc (open-code-binary-proc procsym)))
    (lambda (expr compenv target link)
      (let ((new-expr (rewrite-nary-as-binary expr)))
	(binary-proc new-expr compenv target link)))))

;; Return true if the expr is in the form of (<op> . <vals>)
;; and <op> is in our table, and <op> is not in the compile-time
;; environment, indicating it has been shadowed.
(define (open-coded? expr compenv)
  (and (pair? expr)
       (not (find-variable (car expr) compenv))
       (memq (car expr) (map car open-coded-ops))))

(define (lookup-open-coded-prim oper)
  (define (lookup-inner oper exprs)
    (if (null? exprs) false
	(if (eq? oper (caar exprs))
	    (cadar exprs)
	    (lookup-inner oper (cdr exprs)))))
  (lookup-inner oper open-coded-ops))
  
(define (compile-open-coded expr compenv target link)
  (let ((prim
	 (lookup-open-coded-prim (car expr))))
    (if prim
	(prim expr compenv target link)
	(error "No such primitive to open code -- COMPILE-OPEN-CODED" expr))))



(define (open-code-binary-proc opsym)
  (lambda (expr compenv target link)
    (end-with-linkage
     link
     (spread-arguments (cdr expr)
		       compenv
		       '(arg1 arg2)
		       (make-instruction-sequence
			'(arg1 arg2)
			(list target)
			`((assign ,target (op ,opsym) (reg arg1) (reg arg2))))))))
  
  
(define (open-code-+ expr compenv target link)
  (end-with-linkage
   link
   (spread-arguments (cdr expr)
		     compenv
		     '(arg1 arg2)
		     (make-instruction-sequence
		      '(arg1 arg2)
		      '(val)
		      `((assign ,target (op +) (reg arg1) (reg arg2)))))))
      

(define (spread-arguments ops compenv regs prim-insts)
    (define (spread-inner ops regs old-regs)
      (if (null? ops)
	  prim-insts
	  (preserving
	   (append (cdr regs) old-regs (list 'env 'continue))
	   (compile (car ops) compenv (car regs) 'next)
	   (spread-inner
	    (cdr ops)
	    (cdr regs)
	    (cons (car regs) old-regs)))))
    (spread-inner ops regs '()))

(define open-coded-ops
  `((+ ,(open-code-nary-proc '+))
    (- ,(open-code-nary-proc '-))
    (* ,(open-code-nary-proc '*))
    (= ,(open-code-binary-proc '=))))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences
       seq1 seq2)
      (let ((first-reg (car regs)))
	(if (and (needs-register? seq2 first-reg)
		 (modifies-register? seq1 first-reg))
	    (preserving
	     (cdr regs)
	     (make-instruction-sequence
	      (list-union (list first-reg) (registers-needed seq1))
	      (list-difference (registers-modified seq1) (list first-reg))
	      (append `((save ,first-reg))
		      (statements seq1)
		      `((restore ,first-reg))))
	     seq2)
	    (preserving (cdr regs) seq1 seq2)))))
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
		       (registers-modified seq1)))
     (list-union (registers-modified seq1)
		 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))

  (define (append-seq-list seqs)
    (if (null? seqs)
	(empty-instruction-sequence)
	(append-2-sequences
	 (car seqs)
	 (append-seq-list (cdr seqs)))))

  (append-seq-list seqs))

(define (compile-sequence seq compenv target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) compenv target linkage)
      (preserving '(env continue)
		  (compile (first-exp seq) compenv target 'next)
		  (compile-sequence (rest-exps seq) compenv target linkage))))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
	((memq (car s1) s2) (list-union (cdr s1) s2))
	(else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
	((memq (car s1) s2) (list-difference (cdr s1) s2))
	(else (cons (car s1)
		    (list-difference (cdr s1) s2)))))


  
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
	   (statements body-seq))))

;; (define (parallel-instruction-sequences seq1 seq2)
;;   (make-instruction-sequence
;;    (list-union (registers-needed seq1)
;; 	       (registers-needed seq2))
;;    (list-union (registers-modified seq1)
;; 	       (registers-modified seq2))
;;    (append (statements seq1) (statements seq2))))

(define (parallel-instruction-sequences . seqs)
  (define (union-all . lists)
    (if (null? lists) '()
	(list-union (car lists)
		    (apply union-all (cdr lists)))))
  (make-instruction-sequence
   (apply union-all (map registers-needed seqs))
   (apply union-all (map registers-modified seqs))
   (apply append (map statements seqs))))

;; (define the-global-environment (setup-environment))

;; (define (machine-for-compiled expr)
;;   (let ((code (compile expr the-empty-compile-time-env 'val 'next)))
;;     (let ((code-with-env
;; 	   (cons
;; 	    '(assign env (op get-global-environment))
;; 	    (caddr code))))
;;     (make-machine
;;      '(env val proc argl arg1 arg2 continue)
;;      compiler-operations
;;      code-with-env))))
