(load "polymorph-inheritance.scm")

(define (install-term-package)
  (define (make-term order coeff)
    (list order coeff))
  (define (=term-zero? term) (=zero? (coeff term)))
  (define (tag x) (attach-tag 'poly-term x))
  (put 'make 'poly-term (lambda (order coeff) (tag (make-term order coeff))))
  (put 'order '(poly-term) order)
  (put 'coeff '(poly-term) coeff)
  (put '=zero? '(poly-term) =term-zero?)
  'done)

(define (make-term order coeff) ((get 'make 'poly-term) order coeff))
;; (define (order term) (apply-generic 'order term))
;; (define (coeff term) (apply-generic 'coeff term))
(define (order-untagged term) (car term))
(define (coeff-untagged term) (cadr term))

(define (install-sparse-term-list-package)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (tag-list x) (attach-tag 'sparse-term-list x))
  (define (tag-term x) (attach-tag 'poly-term x))

  (put 'adjoin-term '(poly-term sparse-term-list)
       (lambda (x y) (tag-list (adjoin-term x y))))

  (put 'first-term '(sparse-term-list)
       (lambda (x) (tag-term (first-term x))))
  (put 'rest-terms '(sparse-term-list)
       (lambda (x) (tag-list (rest-terms x))))
  (put 'empty-termlist? '(sparse-term-list)
       empty-termlist?)
  (put 'make 'sparse-term-list (lambda ()
				 (tag-list (the-empty-termlist))))
  'done)

(define (the-empty-sparse-termlist)
  ((get 'make 'sparse-term-list)))

(define make-sparse-termlist the-empty-sparse-termlist)

(define (install-dense-term-list-package)
  (define (adjoin-term term term-list)
    (define (insert-term order coeff list listlen)
      (cond
       ((null? list) (error 
		      "There is a mistake in the insert-term coding -- ADJOIN-TERM"))
       ((= order (- listlen 1)) 
	(cons (add coeff (car list)) (cdr list)))
       (else (cons (car list) (insert-term order coeff (cdr list) (- listlen 1))))))
    (define (repeat-zero n)
      (cond ((= n 0) '())
	    (else (cons 0 (repeat-zero (- n 1))))))
    (let ((term-order (order term))
	  (term-coeff (coeff term)))
      (cond
       ((< term-order (length term-list))
	(insert-term term-order term-coeff term-list (length term-list)))
       (else
	(append (cons term-coeff (repeat-zero (- term-order (length term-list))))
		term-list)))))
  (define (the-empty-termlist) '())
  (define (tag-list x) (attach-tag 'dense-term-list x))
  (define (first-term term-list)
    (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (put 'adjoin-term '(poly-term dense-term-list)
       (lambda (x y) (tag-list (adjoin-term x y))))
  (put 'first-term '(dense-term-list) first-term)
  (put 'rest-terms '(dense-term-list) rest-terms)
  (put 'empty-termlist? '(dense-term-list) empty-termlist?)
  (put 'make 'dense-term-list (lambda ()
				 (tag-list (the-empty-termlist))))
  
  'done)

(define (the-empty-dense-termlist)
  ((get 'make 'dense-term-list)))
(define make-dense-termlist the-empty-dense-termlist)

(define (empty-termlist? term-list)
  (apply-generic 'empty-termlist? term-list))
(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
;; (define (order term)
;;   (apply-generic 'order term))
;; (define (coeff term)
;;   (apply-generic 'coeff term))
(define (adjoin-term term termlist)
  (apply-generic 'adjoin-term term termlist))

(define (same-variable? x y)
  (and (symbol? x) (symbol? y) (eq? x y)))

(define (install-polynomial-package)
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1))
		 (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1
		     (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2
		     (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-sparse-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (=poly-zero? poly)
    (or (empty-termlist? (term-list poly))
	(all-coeffs-zero? poly)))
  (define (all-coeffs-zero? poly)
    (fold-right (lambda (term result) 
		  (and result (=zero? (coeff term))))
		true
		(term-list poly)))
  (put '=zero? '(polynomial) =poly-zero?)
  (define (add-poly p1 p2)
    (cond
     ((same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (add-terms (term-list p1) (term-list p2))))
     (else
      (error "These polys are not in the same variable -- ADD-POLY"
	     p1 p2))))

  (define (mul-poly p1 p2)
    (if
     (same-variable? (variable p1) (variable p2))
     (make-poly (variable p1)
		(mul-terms (term-list p1) (term-list p2)))
     (error
      "These polys cannot be multiplied because they are not in the same variable -- MUL-POLY"
      p1 p2)))
  (define (negate-poly p)
    (make-poly (variable p)
	       (negate-coefficients (term-list p))))
  (define (negate-coefficients terms)
    (if (empty-termlist? terms)
	(the-empty-termlist)
	(let ((first (first-term terms))
	      (rest (rest-terms terms)))
	  (adjoin-term
	   (make-term (order first)
		      (negate (coeff first)))
	   (negate-coefficients rest)))))


  ;; interface to rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))

  'done)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
