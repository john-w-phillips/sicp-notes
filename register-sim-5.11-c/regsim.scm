(load "assembler.scm")

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
		((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value)
	       (set! contents value)))
	    (else
	     (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push reg x)
      (let ((regstack (assoc reg s)))
	(if (not regstack)
	    (error "Bad register: PUSH" reg)
	    (set-cdr! regstack
		    (cons x (cdr regstack))))))
    (define (pop reg)
      (if (null? s)
	  (error "Empty stack: POP")
	  (let ((regstack (assoc reg s)))
	    (cond
	     ((false? regstack)
	      (error "Bad register: POP" reg))
	     ((null? (cdr regstack))
	      (error "Empty register stack: POP"))
	     (else
	      (let ((popped (cadr regstack))
		    (newval (cddr regstack)))
		(set-cdr! regstack newval)
		popped))))))
    (define (initialize regs)
      (set! s
	(map (lambda (rname) (cons rname '()))
	     regs))
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) pop)
	    ((eq? message 'initialize) initialize)
	    (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack reg) ((stack 'pop) reg))
(define (push stack reg item) ((stack 'push) reg item))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    (let ((register-table  (list (list 'pc pc) (list 'flag flag))))
      (let ((the-ops
	     (list (list 'initialize-stack
			 (lambda ()
			   ((stack 'initialize)
			    (map car register-table)))))))
	(define (allocate-register name)
	  (if (assoc name register-table)
	      (error "Multiply defined register: " name)
	      (set! register-table
		(cons (list name (make-register name))
		      register-table)))
	  'register-allocated)
	(define (lookup-register name)
	  (let ((val (assoc name register-table)))
	    (if val
		(cadr val)
		(error "Unknown register: " name))))
	(define (execute)
	  (let ((insts (get-contents pc)))
	    (if (null? insts)
		'done
		(begin
		  ((instruction-execution-proc (car insts)))
		  (execute)))))
	(define (dispatch message)
	  (cond ((eq? message 'start)
		 (set-contents! pc the-instruction-sequence)
		 (execute))
		((eq? message 'install-instruction-sequence)
		 (lambda (seq)
		   (set! the-instruction-sequence seq)))
		((eq? message 'allocate-register)
		 allocate-register)
		((eq? message 'get-register)
		 lookup-register)
		((eq? message 'install-operations)
		 (lambda (ops)
		   (set! the-ops (append the-ops ops))))
		((eq? message 'stack) stack)
		((eq? message 'operations) the-ops)
		(else (error "Unknown request: MACHINE"
			     message))))
	dispatch))))

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)


(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

