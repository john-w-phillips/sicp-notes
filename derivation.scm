(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp)
    (if (same-variable? exp var) 1 0))
   ((sum? exp)
    (make-sum
     (deriv (addend exp) var)
     (deriv (augend exp) var)))
   ((product? exp)
    (make-sum
     (make-product (multiplier exp)
		   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
		   (multiplicand exp))))
   ((and (exponentiation? exp)
	 (number? (exponent exp)))
    (make-product
     (make-product
      (exponent exp)
      (make-exponent
       (base exp)
       (- (exponent exp) 1)))
     (deriv (base exp) var)))
   (else
    (error "unknown expression type -- DERIV" exp))))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (get-left-arg-of-oper oper)
  (define (compile-all-left-of-expr expr)
    (cond
     ((and (pair? expr)
	   (symbol? (car expr))
	   (eq? (car expr) oper))
      '())
     ((pair? expr)
      (cons (car expr)
	    (compile-all-left-of-expr (cdr expr))))
     (else 
      (error 
       "Cannot take the left hand of this expr -- COMPILLE-ALL-LEFT-OF-EXPR"
       expr))))
  (define (get-left-hand-expression expr)
    (let ((left-of (compile-all-left-of-expr expr)))
      (cond
       ((= (length left-of) 1) (car left-of))
       (else left-of))))
  get-left-hand-expression)

(define (get-right-arg-of-oper oper)
  (lambda (expr) 
    (let ((right-of (cdr (memq oper expr))))
      (if (= (length right-of) 1) (car right-of) right-of))))

(define (sum? expr)
  (and (pair? expr) (pair? (memq '+ expr))))

(define addend (get-left-arg-of-oper '+))

(define augend (get-right-arg-of-oper '+))

(define (product? expr)
  (and (pair? expr)
       (not (sum? expr))
       (pair? (memq '* expr))))
(define multiplicand (get-left-arg-of-oper '*))
(define multiplier (get-right-arg-of-oper '*))

(define (exponentiation? expr)
  (and (pair? expr)
       (not (sum? expr))
       (not (product? expr))
       (pair? (memq '** expr))))

(define base (get-left-arg-of-oper '**))
(define exponent (get-right-arg-of-oper '**))


(define (make-exponent base power)
  (cond
   ((=number? power 1) base)
   ((=number? power 0) 1)
   ((=number? base 1) 1)
   ((=number? base 0) 0)
   ((and (exponentiation? base)
	 (exponentiation? power))
    (append base '(**) power))
   ((exponentiation? base)
    (append base '(**) (list power)))
   ((exponentiation? power)
    (append (list base) '(**) power))
   (else
    (list base '** power))))

(define (make-product x y)
  (cond 
   ((or (=number? x 0) (=number? y 0)) 0)
   ((=number? x 1) y)
   ((=number? y 1) x)
   ((and (number? x) (number? y)) (* x y))
   ((and (pair? x) 
	 (pair? y)
	 (not (sum? x))
	 (not (sum? y)))
    (append x '(*) y))
   ((and (pair? x)
	 (not (pair? y))
	 (not (sum? x)))
    (append x '(*) (list y)))
   ((and (pair? x)
	 (pair? y)
	 (not (sum? x))
	 (sum? y))
    (append x '(*) (list y)))
   (else
    (list x '* y))))


(define (make-sum x y)
  (cond
   ((=number? x 0) y)
   ((=number? y 0) x)
   ((and (number? x) (number? y)) (+ x y))
   ((and (pair? x) (pair? y))
    (append x '(+) y))
   (else (list x '+ y))))
