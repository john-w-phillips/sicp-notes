(define UNDEFINED-VARIABLE '*undefined*)

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

(define put-hash (make-hash-table))
(define (put key ns value)
  (let ((ns-table (hash-table-ref put-hash ns (lambda () #f))))
    (if ns-table
	(hash-table-set! ns-table key value)
	(let ((new-hash (make-hash-table)))
	  (hash-table-set! put-hash ns new-hash)
	  (hash-table-set! new-hash key value)))))

(define (get key ns)
  (let ((ns-table (hash-table-ref put-hash ns (lambda () #f))))
    (if ns-table
	(hash-table-ref ns-table key (lambda () #f))
	#f)))

(define primitive-environment
  (extend-environment
   (list
    'list
    'memq
    'member
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
    'false?
    'eq?
    'get
    'put
    'sqrt
    'remainder
    'display
    'newline
    'flush-output
    'pair?
    'assoc
    'symbol->string
    'string->symbol
    'string-append
    'cadr
    'caddr
    'caadr
    'cdar
    'cddar
    'equal?
    'number?
    'number->string
    'string=?
    'symbol?
    'substring
    'map
    'filter
    'error
    'read)
   (list
    (make-primitive-procedure list)
    (make-primitive-procedure memq)
    (make-primitive-procedure member)
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
    (make-primitive-procedure false?)
    (make-primitive-procedure eq?)
    (make-primitive-procedure get)
    (make-primitive-procedure put)
    (make-primitive-procedure sqrt)
    (make-primitive-procedure remainder)
    (make-primitive-procedure display)
    (make-primitive-procedure newline)
    (make-primitive-procedure flush-output)
    (make-primitive-procedure pair?)
    (make-primitive-procedure assoc)
    (make-primitive-procedure symbol->string)
    (make-primitive-procedure string->symbol)
    (make-primitive-procedure string-append)
    (make-primitive-procedure cadr)
    (make-primitive-procedure caddr)
    (make-primitive-procedure caadr)
    (make-primitive-procedure cdar)
    (make-primitive-procedure cddar)
    (make-primitive-procedure equal?)
    (make-primitive-procedure number?)
    (make-primitive-procedure number->string)
    (make-primitive-procedure string=?)
    (make-primitive-procedure symbol?)
    (make-primitive-procedure substring)
    (make-primitive-procedure map)
    (make-primitive-procedure filter)
    (make-primitive-procedure error)
    (make-primitive-procedure read))
   the-empty-environment))

(define (setup-environment)
  (let ((initial-env primitive-environment))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
