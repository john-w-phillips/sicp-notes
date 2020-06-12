(load "../interpreter/syntax.scm")
(load "../eceval-impl/environment.scm")
(load "../eceval-impl/primitive-apply.scm")
(load "../eceval-impl/assembler.scm")
(load "../eceval-impl/regsim.scm")
(load "../eceval-impl/eceval-ops.scm")
(load "../eceval-impl/syntax.scm")

(define (get-global-environment)
  the-global-environment)

(define the-global-environment (setup-environment))

(define (make-compiled-procedure entry-label env)
  (list entry-label env))

(define (compiled-procedure-entry proc)
  (car proc))

(define (compiled-procedure-env proc)
  (cadr proc))

(define *label-num* 1)
(define all-regs '(val exp proc continue))

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

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
	 (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp) (compile-variable exp target linkage))
	((assignment? exp)
	 (compile-assignment exp target linkage))
	((definition? exp)
	 (compile-definition exp target linkage))
	((if? exp) (compile-if exp target linkage))
	((lambda? exp) (compile-lambda exp target linkage))
	((begin? exp)
	 (compile-sequence
	  (begin-actions exp)
	  target linkage))
	((cond? exp)
	 (compile (cond->if exp) target linkage))
	((open-coded? exp)
	 (compile-open-coded exp target linkage))
	((application? exp)
	 (compile-application exp target linkage))
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

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '()
			      (list target)
			      `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
			      `((assign ,target
					(const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) (list target)
    `((assign ,target
	      (op lookup-variable-value)
	      (const ,exp)
	      (reg env))))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
	(get-value-code
	 (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op set-variable-value!)
		  (const ,var)
		  (reg val)
		  (reg env))
	 (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
	(get-value-code
	 (compile (definition-value exp) 'val 'next)))
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

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
	(f-branch (make-label 'false-branch))
	(after-if (make-label 'after-if)))
    (let ((consequent-linkage
	   (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
	    (c-code
	     (compile
	      (if-consequent exp) target consequent-linkage))
	    (a-code (compile (if-alternative exp) target linkage)))
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

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
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
     (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-lambda exp target linkage)
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
	(compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
	(operand-codes
	 (map (lambda (operand) (compile operand 'val 'next))
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
	 (code-for-rest-args (cdr operand-codes))))))
		    
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage
	   (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
	'(proc) '()
	`((test (op primitive-procedure?) (reg proc))
	  (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
	(append-instruction-sequences
	 compiled-branch
	 (compile-proc-appl target compiled-linkage))
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
    (lambda (expr target link)
      (let ((new-expr (rewrite-nary-as-binary expr)))
	(binary-proc new-expr target link)))))

(define (open-coded? expr)
  (if (pair? expr)
      (memq (car expr) (map car open-coded-ops))
      false))

(define (lookup-open-coded-prim oper)
  (define (lookup-inner oper exprs)
    (if (null? exprs) false
	(if (eq? oper (caar exprs))
	    (cadar exprs)
	    (lookup-inner oper (cdr exprs)))))
  (lookup-inner oper open-coded-ops))
  
(define (compile-open-coded expr target link)
  (let ((prim
	 (lookup-open-coded-prim (car expr))))
    (if prim
	(prim expr target link)
	(error "No such primitive to open code -- COMPILE-OPEN-CODED" expr))))



(define (open-code-binary-proc opsym)
  (lambda (expr target link)
    (end-with-linkage
     link
     (spread-arguments (cdr expr)
		       '(arg1 arg2)
		       (make-instruction-sequence
			'(arg1 arg2)
			'(val)
			`((assign ,target (op ,opsym) (reg arg1) (reg arg2))))))))
  

(define (open-code-+ expr target link)
  (end-with-linkage
   link
   (spread-arguments (cdr expr)
		     '(arg1 arg2)
		     (make-instruction-sequence
		      '(arg1 arg2)
		      '(val)
		      `((assign ,target (op +) (reg arg1) (reg arg2)))))))
      

(define (spread-arguments ops regs prim-insts)
    (define (spread-inner ops regs old-regs)
      (if (null? ops)
	  prim-insts
	  (preserving
	   (append (cdr regs) old-regs)
	   (compile (car ops) (car regs) 'next)
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

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
		  (compile (first-exp seq) target 'next)
		  (compile-sequence (rest-exps seq) target linkage))))

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

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
	       (registers-needed seq2))
   (list-union (registers-modified seq1)
	       (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;;; Stuff for running in a register simulator.
(define compiler-operations
  (list (list 'empty-arglist empty-arglist)
	(list 'read read)
	(list '+ +)
	(list '- -)
	(list '= =)
	(list '* *)
	(list 'make-compiled-procedure make-compiled-procedure)
	(list 'compiled-procedure-env compiled-procedure-env)
	(list 'compiled-procedure-entry compiled-procedure-entry)
	(list 'adjoin-arg adjoin-arg)
	(list 'last-operand? last-operand?)
	(list 'prompt-for-input prompt-for-input)
	(list 'announce-output announce-output)
	(list 'clear-output clear-output)
	(list 'user-print user-print)
	(list 'quoted? quoted?)
	(list 'extract-error-description extract-error-description)
	(list 'null? null?)
	(list 'variable? variable?)
	(list 'assignment? assignment?)
	(list 'definition? definition?)
	(list 'if? if?)
	(list 'symbol? symbol?)
	(list 'lambda? lambda?)
	(list 'begin? begin?)
	(list 'list list)
	(list 'car car)
	(list 'cons cons)
	(list 'cdr cdr)
	(list 'reverse reverse)
	(list 'application? application?)
	(list 'lookup-variable-value lookup-variable-value)
	(list 'text-of-quotation text-of-quotation)
	(list 'lambda-parameters lambda-parameters)
	(list 'lambda-body lambda-body)
	(list 'make-procedure make-procedure)
	(list 'get-global-environment get-global-environment)
	(list 'set-variable-value! set-variable-value!)
	(list 'definition-variable definition-variable)
	(list 'definition-value definition-value)
	(list 'self-evaluating? self-evaluating?)
	(list 'operands operands)
	(list 'first-operand first-operand)
	(list 'rest-operands rest-operands)
	(list 'primitive-procedure? primitive-procedure?)
	(list 'compound-procedure? compound-procedure?)
	(list 'apply-primitive-procedure apply-primitive-procedure)
	(list 'operator operator)
	(list 'procedure-parameters procedure-parameters)
	(list 'procedure-environment procedure-environment)
	(list 'procedure-body procedure-body)
	(list 'extend-environment extend-environment)
	(list 'begin-actions begin-actions)
	(list 'first-exp first-exp)
	(list 'rest-exps rest-exps)
	(list 'last-exp? last-exp?)
	(list 'if-predicate if-predicate)
	(list 'if-consequent if-consequent)
	(list 'if-alternative if-alternative)
	(list 'true? true?)
	(list 'false? false?)
	(list 'assignment-variable assignment-variable)
	(list 'assignment-value assignment-value)
	(list 'no-operands? no-operands?)
	(list 'define-variable! define-variable!)))

(define the-global-environment (setup-environment))

(define (machine-for-compiled expr)
  (let ((code (compile expr 'val 'next)))
    (make-machine
     '(env val proc argl arg1 arg2 continue)
     compiler-operations
     (caddr code))))
