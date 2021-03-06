(define UNDEFINED-VARIABLE '*undefined-interp2*)

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
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define the-empty-environment '()) 
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (find-cell-in-frame var frame)
  (if (not (and (pair? frame)
		(eq? (car frame) 'frame)))
      (error "Not a frame: " frame))
  (filter (lambda (x) (eq? (car x) var))
	  (frame-variable-value-pairs frame)))

(define (set-cell-val! val cell)
  (set-cdr! (car cell) val))

(define (cell-val cell)
  (cdar cell))

(define (null-cell? cell) (null? cell))

(define (operate-on-cell proc var env)
  (if (eq? env the-empty-environment)
      (error "Variable has no definition or entry -- OPERATE-ON-CELL" var)
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
	(error "Variable value is undefined -- LOOKUP-VARIABLE-VALUE"
	       var)
	(if (promise? found-value)
	    (force found-value)
	    found-value))))

(define (set-variable-value! var val env)
  (operate-on-cell
   (lambda (cell)
     (set-cell-val! val cell))
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
    (make-primitive-procedure +)
    (make-primitive-procedure *)
    (make-primitive-procedure =)
    (make-primitive-procedure -)
    (make-primitive-procedure car)
    (make-primitive-procedure cons)
    (make-primitive-procedure cdr)
    (make-primitive-procedure get-universal-time)
    (make-primitive-procedure null?)
    (make-primitive-procedure >)
    (make-primitive-procedure <)
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
