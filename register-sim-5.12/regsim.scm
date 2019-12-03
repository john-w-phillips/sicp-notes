(load "assembler.scm")

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
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
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack: POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack item) ((stack 'push) item))
(define (make-control-data)
  (let ((controlhash (make-hash-table)))
    (define (insert-a-pair itemvalue alist)
      (cond
       ((null? alist)
	(list itemvalue))
       ((and (pair? (car alist))
	     (eq? (caar alist) (car itemvalue)))
	(cons itemvalue alist))
       (else
	(cons (car alist)
	      (insert-a-pair
	       itemvalue
	       (cdr alist))))))
    (define (insert-value itemvalue alist)
      (if (not (false? (member itemvalue alist)))
	  alist
	  (if (pair? itemvalue)
	      (insert-a-pair itemvalue alist)
	      (cons itemvalue alist))))
    (define (insert-keyed-item itemtype itemkey itemvalue)
      (let ((the-alist
	     (hash-table-ref controlhash itemtype (lambda () '()))))
	(let ((values (assoc itemkey the-alist)))
	  (cond
	   ((false? values)
	    (hash-table-set!
	     controlhash
	     itemtype
	     (cons (cons itemkey (list itemvalue)) the-alist)))
	   (else
	    (if (false? (member itemvalue (cdr values)))
		(set-cdr!
		 values (cons itemvalue (cdr values)))))))))
    (define (get-keyed-table itemtype itemkey)
      (let ((the-alist
	     (hash-table-ref controlhash itemtype (lambda () '()))))
	(let ((found-item (assoc itemkey the-alist)))
	  (if found-item
	      (cdr found-item)
	      '()))))
    (define (print-tables)
      (let ((keys (hash-table-keys controlhash)))
	(for-each
	 (lambda (key)
	   (display "Table ")
	   (display key)
	   (newline)
	   (pp (hash-table-ref controlhash key))
	   (newline))
	 keys)))
    (define (insert-item itemtype itemvalue)
      (let ((thelist
	     (hash-table-ref
	      controlhash
	      itemtype
	      (lambda () '()))))
	(hash-table-set!
	 controlhash
	 itemtype
	 (insert-value
	  itemvalue
	  thelist))))
    (define (get-table itemtype)
      (hash-table-ref controlhash itemtype (lambda () '())))
    (define (dispatch msg)
      (cond ((eq? msg 'insert-item!)
	     insert-item)
	    ((eq? msg 'insert-keyed-item!)
	     insert-keyed-item)
	    ((eq? msg 'get-table)
	     get-table)
	    ((eq? msg 'get-keyed-table)
	     get-keyed-table)
	    ((eq? msg 'print-tables)
	     (print-tables))
	    (else
	     (error "Can't recognize message CONTROL-DATA" msg))))
    dispatch))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(control-data (make-control-data))
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
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
	      ((eq? message 'has-register?)
	       (lambda (name)
		 (not (false? (assoc name register-table)))))
	      ((eq? message 'get-control-data)
	       control-data)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'print-control-data)
	       (control-data 'print-tables))
	      (else (error "Unknown request: MACHINE"
			   message))))
      dispatch)))

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)


(define (get-or-create-register machine reg-name)
  (if ((machine 'has-register?) reg-name)
      ((machine 'get-register) reg-name)
      (begin
	(display "alloc ")
	(display reg-name) (newline)
	((machine 'allocate-register) reg-name)
	((machine 'get-register) reg-name))))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


