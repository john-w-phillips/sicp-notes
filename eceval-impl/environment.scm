(define UNDEFINED-VARIABLE '*undefined*)

(define (make-primitive-procedure proc)
  (list 'primitive-procedure proc))
(define (primitive-procedure-proc proc) (cadr proc))

(define (remove-bindings-from-frame var frame)
  (filter (lambda (binding) (not (eq? (car binding) var)))
	  frame))
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (let ((lookup (find-cell-in-frame var frame)))
      (if (null? lookup)
	  (make-unbound! var (enclosing-environment env))
	  (set-car! env (remove-bindings-from-frame var frame))))))

(define (make-frame variables values)
  (list 'frame (map cons variables values)))
(define (frame-variables frame) (map car (cadr frame)))
(define (frame-values frame) (map cdr (cadr frame)))
(define (frame-variable-value-pairs frame)
  (cadr frame))
(define (frame-has-values? frame)
  (not (null? (cadr frame))))

(define (add-binding-to-frame! var val frame)
  (if (frame-has-values? frame)
      (let ((last-cell (last-pair (frame-variable-value-pairs frame))))
	(set-cdr! last-cell (list (cons var val))))
      (set-car! (cdr frame) (list (cons var val)))))

;; extend-environment is the same.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (make-error "Too many arguments supplied")
	  (make-error "Too few arguments supplied"))))

(define the-empty-environment '()) 
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (find-cell-in-frame var frame)
  (filter (lambda (x) (eq? (car x) var))
	  (frame-variable-value-pairs frame)))

(define (set-cell-val! val cell)
  (set-cdr! (car cell) val))

(define (cell-val cell)
  (cdar cell))

(define (null-cell? cell) (null? cell))

(define (operate-on-cell proc var env)
  (if (eq? env the-empty-environment)
      (make-error
       "Variable has no definition or entry -- OPERATE-ON-CELL")
      (let ((lookup (find-cell-in-frame var (first-frame env))))
	(if (null-cell? lookup)
	    (operate-on-cell proc var (enclosing-environment env))
	    (proc lookup)))))

(define (lookup-variable-value var env)
  (let ((found-value (operate-on-cell
		      (lambda (cell)
			(cell-val cell))
		      var env)))
    (if (eq? found-value UNDEFINED-VARIABLE)
	(make-error "Variable value is undefined -- LOOKUP-VARIABLE-VALUE")
	(if (promise? found-value)
	    (force found-value)
	    found-value))))

(define (set-variable-value! var val env)
  (operate-on-cell
   (lambda (cell)
     (set-cell-val! val cell)
     'ok)
   var env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((lookup (find-cell-in-frame var frame)))
	  (if (null? lookup)
	      (add-binding-to-frame! var val frame) 
	      (set-cdr! (car lookup) val)))))

(define (false? value)
  (or (eq? value #f)
      (eq? value 'false)))

(define (falsy? value)
  (or (false? value)
      (null? value)
      (eq? value 0)))


(define (true? value)
  (or (eq? value #t)
      (eq? value 'true)))

(define (truthy? value)
  (or (true? value)
      (not (falsy? value))))

(define (car-wrap . alist)
  (cond
   ((not (= (length alist) 1))
    (make-error "CAR requires one argument"))
   ((not (pair? (car alist)))
    (make-error "CAR -- Not a list"))
   (else (car (car alist)))))

(define (cdr-wrap . alist)
  (cond
   ((not (= (length alist) 1))
    (make-error "CDR requires one argument")) 
   ((not (pair? (car alist)))
    (make-error "CDR -- not a list"))
   (else (cdr (car alist)))))

(define (make-binary-wrap op)
  (lambda (a b)
    (cond
     ((and (number? a) (number? b))
      (op a b))
     (else (make-error "Not a number")))))

(define (make-list-operator error-message initial test binop)
  (define (iter result rest)
    (cond
     ((null? rest) result)
     ((test (car rest))
      (iter (binop result (car rest))
	    (cdr rest)))
     (else
      (make-error error-message))))

  (define (op-replacement . args)
    (iter initial args))
  op-replacement)

(define (sub-wrap . args)
  (define (iterate-list first rest)
    (cond
     ((null? rest) first)
     (else
      (iterate-list (- first (car rest))
		    (cdr rest)))))
  (cond
   ((null? args) 0)
   ((null? (cdr args)) (- 0 (car args)))
   (else
    (iterate-list (car args) (cdr args)))))

(define (cons-wrap . args)
  (cond
   ((not (= (length args) 2))
    (make-error "CONS requires exactly two arguments"))
   (else
    (cons (car args) (cadr args)))))

(define primitive-environment
  (extend-environment
   (list
    '+
    '*
    '=
    '-
    'car 
    'cons 
    'cdr
    'get-universal-time
    'null?
    '>
    '<
    'not
    'true?
    'false?)
   (list
    (make-primitive-procedure (make-list-operator
			       "Not a number"
			       0
			       number?
			       +))
    (make-primitive-procedure (make-list-operator
			       "Not a number"
			       1
			       number?
			       *))
    (make-primitive-procedure (make-binary-wrap =))
    (make-primitive-procedure sub-wrap)
    (make-primitive-procedure car-wrap)
    (make-primitive-procedure cons-wrap)
    (make-primitive-procedure cdr-wrap)
    (make-primitive-procedure get-universal-time)
    (make-primitive-procedure null?)
    (make-primitive-procedure (make-binary-wrap >))
    (make-primitive-procedure (make-binary-wrap <))
    (make-primitive-procedure not)
    (make-primitive-procedure true?)
    (make-primitive-procedure false?))
   the-empty-environment))

(define (setup-environment)
  (let ((initial-env primitive-environment))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
