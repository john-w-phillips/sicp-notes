(load "polymorph-inheritance.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (add4 a b c d) (apply-generic 'add4 a b c d))

(define (install-integer)
  (define (tag x)
    (attach-tag 'integer x))
  (put-projection 'rational
		  (lambda (x) (floor (/ (numer x) (denom x)))))
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (attach-tag 'rational (make-rational x y))))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (define (scheme-number-equ? x y)
    (= x y))
  (put 'equ? '(integer integer) scheme-number-equ?)
  (put-raise 'integer 'rational (lambda (integer)
				  (make-rational integer 1)))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (=zero-rational? x)
    (= 0 (numer x)))
  (define MAX-DIGITS 15)
  (define (coerce-to-rational areal)
    (define (coerce-to-rational-inner num denom digit-counter)
      (cond
       ((or (= digit-counter 0)
	    (= (floor num) num))
	(make-rational (inexact->exact (floor num)) (inexact->exact (floor denom))))
       (else
	(coerce-to-rational-inner (* num 10)  (* 10 denom) (- digit-counter 1)))))
    (coerce-to-rational-inner areal 1 MAX-DIGITS))
  
  (put-projection 'real coerce-to-rational)

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  (define (rational-equ? x y)
    (and (= (numer x) (numer y))
	 (= (denom x) (denom y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put '=zero? '(rational) =zero-rational?)
  (put 'equ? '(rational rational) rational-equ?)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y)(tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(put-raise 'rational 'real
	   (lambda (x) (make-real (* 1.0 (exact->inexact (/ (numer x) (denom x)))))))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer n)
  (apply-generic 'numer n))
(define (denom n)
  (apply-generic 'denom n))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put-projection 'complex (lambda (x) (* 1.0 (real-part x))))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (/ x y)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (define (real-equ? x y)
    (= x y))
  (put 'equ? '(real real) real-equ?)
  (put-raise 'real 'complex (lambda (real)
			      (make-complex-from-real-imag real 0)))
  'done)

(define (make-real x)
  ((get 'make 'real) x)) 

(define (install-complex-package)
  (load "complex.scm")
  (install-polar-package)
  (install-rectangular-package)
  ;; imported procedur es from rectangular and polar packages
  (define (make-from-real-imag x y) 
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add4 a b c d)
    (make-from-real-imag
     (+ (real-part a)
	(real-part b)
	(real-part c)
	d)
     (+ (imag-part a)
	(imag-part b)
	(imag-part c))))
  (define (=zero-complex? x)
    (and (= 0 (real-part x)) (= 0 (imag-part x))))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  (define (complex-equ? x y)
    (and (= (real-part x) (real-part y))
	 (= (imag-part x) (imag-part y))))
  ;; interface to the rest of the system 
  (define (tag z) (attach-tag 'complex z))
  (put '=zero? '(complex) =zero-complex?)
  (put 'equ? '(complex complex) complex-equ?)
  (put 'add4 '(complex complex complex integer)
       (lambda (x1 x2 x3 x4) (tag (add4 x1 x2 x3 x4))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex)
       (lambda (x) (apply-generic 'real-part x)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (equ? x y)
  (apply-generic 'equ? x y))
