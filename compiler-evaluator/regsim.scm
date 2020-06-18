(load "assembler.scm")

(define ERROR-TAG "error")

(define (make-error error-description)
  (list ERROR-TAG error-description))

(define (machine-error? data)
  (and (pair? data)
       (eq? (car data) ERROR-TAG)))

(define (error-description data)
  (cadr data))
(define (extract-error-description data)
  (cond
   ((and (pair? data)
	 (machine-error? data)
	 (> (length data) 1))
    (error-description data))
   (else
    "")))

(define (is-frame? value)
  (and (pair? value)
       (eq? (car value) 'frame)))

(define (check-reg-is-env reg-contents)
  (or (null? reg-contents)
      (and (is-frame? (car reg-contents))
	   (check-reg-is-env (cdr reg-contents)))))
		      
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
  (let ((contents '*unassigned*)
	(tracing false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'trace-on)
	     (set! tracing true)
	     'ok)
	    ((eq? message 'trace-off)
	     (set! tracing false)
	     'ok)
	    ((eq? message 'set)
	     (lambda (value)
	       (if tracing
		   (begin
		     ;;(newline)
		     ;; (display (list 'register: name 'change 'old-value '= contents
		     ;; 		    'new-value '= value))
		     (if (not (check-reg-is-env value))
			 (error "Set to non-env value")
			 (display "reg ok\n")
			 )))
	       (set! contents value)))
	    (else
	     (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

(define (make-stack)
  (let ((s '())
	(number-pushes 0)
	(max-depth 0)
	(current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack: POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    (set! current-depth (- current-depth 1))
	    top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
		     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'print-statistics) (print-statistics))
	    ((eq? message 'initialize) (initialize))
	    (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack item) ((stack 'push) item))

(define (any a-list)
  (cond
   ((null? a-list) false)
   (else
    (or (and (not (false? (car a-list)))
	     (not (null? (car a-list))))
	(any (cdr a-list))))))

(define (make-breakpoint brk-label brk-inst-number)
  (cons brk-label brk-inst-number))

(define (breakpoint-label brk) (car brk))
(define (breakpoint-inst-number brk) (cdr brk))

(define (is-this-break-set? inst labels brk)
  (let ((label (assoc (breakpoint-label brk) labels)))
    ;; (display "checking item ")
    ;; (display (list-ref label (breakpoint-inst-number brk)))
    ;; (newline)
    ;; (display "against ")
    ;; (display inst)
    ;; (newline)
    (and label
	 (equal? (list-ref 
		  (label-entry-insts label)
		  (breakpoint-inst-number brk))
		 inst))))

(define (is-breakpoint-set?
	 inst
	 labels
	 breakpoints)
  (any (map (lambda (brk) (is-this-break-set? inst labels brk))
	    breakpoints)))

	    
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(do-tracing false)
	(instruction-counter 0)
	(preceding-label-count 1)
	(breakpoints '())
	(break-flag false)
	(labels '())
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))
		 (list 'machine-error?
		       machine-error?)
		 (list 'print-statistics
		       (lambda () (stack 'print-statistics)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
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
      (define (print-preceding-labels label-list count)
	(cond
	 ((or (= count 0) (null? label-list)) (newline))
	 (else
	  (begin
	    (display (car label-list))
	    (display " ")
	    (print-preceding-labels (cdr label-list) (- count 1))))))
      (define (cancel-a-breakpoint label n)
	(set! breakpoints
	  (filter (lambda (brk)
		    (not (and (eq? (breakpoint-label brk) label)
			      (= (breakpoint-inst-number brk) n))))
		    breakpoints)))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		(let ((inst (car insts)))
		  (if do-tracing
		      (begin
			(newline)
			(display "Labels: ")
			(print-preceding-labels
			 (reverse (instruction-preceding-labels inst))
			 preceding-label-count)
			(newline)
			(display "Instruction: ")
			(display (instruction-text inst))))
		  (if (and (not (eq? break-flag true))
			   (is-breakpoint-set?
			    inst
			    labels
			    breakpoints))
		      (begin
			(set! break-flag true)
			'break)
		      (begin
			(set! break-flag false)
			((instruction-execution-proc inst))
			(set! instruction-counter (+ instruction-counter 1))
			(execute))))))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'print-breaks)
	       (newline)
	       (display breakpoints))
	      ((eq? message 'resume)
	       (execute))
	      ((eq? message 'set-breakpoint)
	       (lambda (brklabel brkoff)
		 (set! breakpoints
		       (cons (make-breakpoint
			      brklabel brkoff)
			     breakpoints))))
	      ((eq? message 'cancel-all-breakpoints)
	       (set! breakpoints '())
	       'ok)
	      ((eq? message 'cancel-a-breakpoint)
	       cancel-a-breakpoint)
	      ((eq? message  'install-labels)
	       (lambda (the-labels)
		 (set! labels the-labels)
		 'ok))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'set-trace-label-depth)
	       (lambda (depth)
		 (set! preceding-label-count depth)))
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'instruction-count)
	       instruction-counter)
	      ((eq? message 'set-labels)
	       (lambda (new-labels)
		 (set! labels new-labels)
		 'ok))
	      ((eq? message 'reset-instruction-counter)
	       (set! instruction-counter 0)
	       'ok)
	      ((eq? message 'stack) stack)
	      ((eq? message 'trace-on)
	       (set! do-tracing true)
	       'ok)
	      ((eq? message 'set-register-trace)
	       (lambda (regname)
		 (let ((register (lookup-register regname)))
		   (register 'trace-on))))
	      ((eq? message 'stop-register-trace)
	       (lambda (regname)
		 (let ((register (lookup-register regname)))
		   (register 'trace-off))))
	      ((eq? message 'trace-off)
	       (set! do-tracing false))
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request: MACHINE"
			   message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (show-breakpoints machine) (machine 'print-breaks))
(define (proceed-machine machine) (machine 'resume))
(define (set-breakpoint machine label offset)
  ((machine 'set-breakpoint) label offset))

(define (cancel-a-breakpoint machine label n)
  ((machine 'cancel-a-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)


(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


