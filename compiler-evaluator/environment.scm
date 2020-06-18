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
(define (vars-has-rest? vars)
  (cond ((null? vars) false)
	((not (pair? vars)) true)
	((and
	  (not (null? (cdr vars)))
	  (not (pair? (cdr vars))))
	 true)
	(else (vars-has-rest? (cdr vars)))))

(define (take-n n alist)
  (cond
   ((= n 0) '())
   (else
    (cons (car alist) (take-n (- n 1) (cdr alist))))))
(define (nth-cdr n alist)
  (if (= n 0) alist
      (nth-cdr (- n 1) (cdr alist))))
(define (listify-restlist vars)
  (cond ((and (not (null? vars))
	      (not (pair? vars)))
	 (list vars))
	(else
	 (cons (car vars) (listify-restlist (cdr vars))))))
(define (nonlist-length nonlist)
  (cond
   ((not (pair? nonlist)) 0)
   (else
    (+ 1 (nonlist-length (cdr nonlist))))))

(define (extend-environment vars vals base-env)
  (cond
   ((vars-has-rest? vars)
    (let ((first-vals (take-n (nonlist-length vars) vals))
	  (rest-vals (nth-cdr (nonlist-length vars) vals)))
      (let ((vars (listify-restlist vars))
	    (vals (append first-vals (list rest-vals))))
	(extend-environment vars vals base-env))))
   (else
    (if (= (length vars) (length vals))
	(cons (make-frame vars vals) base-env)
	(if (< (length vars) (length vals))
	    (make-error "Too many arguments supplied")
	    (make-error "Too few arguments supplied"))))))
	
      
;; (define (extend-environment vars vals base-env)
;;   (if (= (length vars) (length vals))
;;       (cons (make-frame vars vals) base-env)
;;       (if (< (length vars) (length vals))
;; 	  (make-error "Too many arguments supplied")
;; 	  (make-error "Too few arguments supplied"))))

(define the-empty-environment '()) 
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (find-cell-in-frame var frame)
  (if (not (and (pair? frame)
		(eq? (car frame) 'frame)))
      (error "Not a frame: " var frame))
  (filter (lambda (x) (eq? (car x) var))
	  (frame-variable-value-pairs frame)))

(define (set-cell-val! val cell)
  (set-cdr! (car cell) val))

(define (cell-val cell)
  (cdar cell))

(define (null-cell? cell) (null? cell))

(define (operate-on-cell proc var env)
  (if (eq? env the-empty-environment)
      (error
       (list "Variable has no definition or entry -- OPERATE-ON-CELL" var))
      (let ((lookup (find-cell-in-frame var (first-frame env))))
	(if (null-cell? lookup)
	    (operate-on-cell proc var (enclosing-environment env))
	    (proc lookup)))))

(define (lookup-variable-value var env)
  (if (not (check-reg-is-env env))
      (error "NOt an environment" env))
  (let ((found-value (operate-on-cell
		      (lambda (cell)
			(cell-val cell))
		      var env)))
    (if (eq? found-value UNDEFINED-VARIABLE)
	(error
	 (list "Variable value is undefined -- LOOKUP-VARIABLE-VALUE" var))
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
    (error "CAR requires one argument"))
   ((not (pair? (car alist)))
    (error "CAR -- Not a list"))
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
    'false?
    'display
    'newline
    'read
    'list
    'length
    'caddr
    'cadr
    'caar
    'cadar
    'pair?
    'eq?
    'set-cdr!
    'number?
    'symbol?
    'string?
    'boolean?
    'cdar
    'cddar
    'cdaar
    'cadddr
    'cddr
    'cdddr
    'cdadr
    'caadr
    'caaddr
    'cdaddr)
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
    (make-primitive-procedure false?)
    (make-primitive-procedure display)
    (make-primitive-procedure newline)
    (make-primitive-procedure read)
    (make-primitive-procedure list)
    (make-primitive-procedure length)
    (make-primitive-procedure caddr)
    (make-primitive-procedure cadr)
    (make-primitive-procedure caar)
    (make-primitive-procedure cadar)
    (make-primitive-procedure pair?)
    (make-primitive-procedure eq?)
    (make-primitive-procedure set-cdr!)
    (make-primitive-procedure number?)
    (make-primitive-procedure symbol?)
    (make-primitive-procedure string?)
    (make-primitive-procedure boolean?)
    (make-primitive-procedure cdar)
    (make-primitive-procedure cddar)
    (make-primitive-procedure cdaar)
    (make-primitive-procedure cadddr)
    (make-primitive-procedure cddr)
    (make-primitive-procedure cdddr)
    (make-primitive-procedure cdadr)
    (make-primitive-procedure caadr)
    (make-primitive-procedure caaddr)
    (make-primitive-procedure cdaddr))
   the-empty-environment))



