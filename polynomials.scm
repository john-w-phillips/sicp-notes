(load "polymorph-inheritance.scm")
(define VAR-ORDER
  '(x y z t u v w a b c p q))

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
(define (make-untagged-term order coeff) (list order coeff))
(define (make-term order coeff) ((get 'make 'poly-term) order coeff))
;; (define (order term) (apply-generic 'order term))
;; (define (coeff term) (apply-generic 'coeff term))
(define (order term)
  (if (symbol? (car term))
      (cadr term)
      (car term)))
(define (coeff term)
  (if (symbol? (car term))
      (caddr term)
      (cadr term)))

(define (install-sparse-term-list-package)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (term-list-length term-list) (length term-list))
  (define (tag-list x) (attach-tag 'sparse-term-list x))
  (define (tag-term x) (attach-tag 'poly-term x))

  (put 'adjoin-term '(poly-term sparse-term-list)
       (lambda (x y) (tag-list (adjoin-term x y))))

  (put 'first-term '(sparse-term-list)
       (lambda (x) (first-term x)))
  (put 'term-list-length '(sparse-term-list)
       term-list-length)
  (put 'rest-terms '(sparse-term-list)
       (lambda (x) (tag-list (rest-terms x))))
  (put 'empty-termlist? '(sparse-term-list)
       empty-termlist?)
  (put 'make 'sparse-term-list (lambda ()
				 (tag-list (the-empty-termlist))))
  'done)

(define (the-empty-sparse-termlist)
  ((get 'make 'sparse-term-list)))
(define the-empty-termlist the-empty-sparse-termlist)
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
    (contents (make-term (- (length term-list) 1) (car term-list))))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (term-list-length term-list) (length term-list))

  (put 'adjoin-term '(poly-term dense-term-list)
       (lambda (x y) (tag-list (adjoin-term x y))))
  (put 'first-term '(dense-term-list) first-term)
  (put 'rest-terms '(dense-term-list) (lambda (x) (tag-list (rest-terms x))))
  (put 'empty-termlist? '(dense-term-list) empty-termlist?)
  (put 'make 'dense-term-list (lambda ()
				(tag-list (the-empty-termlist))))
  (put 'term-list-length '(dense-term-list) term-list-length)
  
  'done)

(define (the-empty-dense-termlist)
  ((get 'make 'dense-term-list)))
(define make-dense-termlist the-empty-dense-termlist)

(define (term-list-length term-list)
  (apply-generic 'term-list-length term-list))

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
  (install-term-package)
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1))
		 (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     (make-term (order t1)
				(coeff t1))
		     (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     (make-term (order t2)
				(coeff t2))
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
	;; (the-empty-termlist)
	L ;; this way we don't have a prefferred/hardcoded type of return list.
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
	;; (list (the-empty-termlist) (the-empty-termlist))
	(list L1 L1)
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order t2) (order t1))
	      (list (the-empty-sparse-termlist) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1) (order t2))))
		(let ((rest-of-result
		       (div-terms 
			(add-terms 
			 L1
			 (negate-coefficients 
			  (mul-term-by-all-terms 
			   (make-untagged-term new-o new-c)
			   L2)))
			L2)))
		  (let ((remainder (cadr rest-of-result))
			(div-result (car rest-of-result)))
		    (list
		     (adjoin-term
		      (make-term new-o new-c)
		      div-result)
		     remainder))))))))

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

  ;; is v1 higher than v2?
  (define (higher-variable? v1 v2)
    (false? (memq v1 (memq v2 VAR-ORDER))))

  (define (coerce-to-variable variable poly)
    (transform-terms variable poly))

  (define (coerce-to-highest-variable-pair poly1 poly2)
    (if (same-variable? (variable poly1) (variable poly2))
	(cons poly1 poly2)
	(cond
	 ((higher-variable? (variable poly1) (variable poly2))
	  (cons poly1 (contents (coerce-to-variable (variable poly1) poly2))))
	 (else (cons (contents (coerce-to-variable (variable poly2) poly1)) poly2)))))

  (define (add-to-coefficient-from-poly
	   new-var
	   new-term
	   poly-term)
    (let ((term-order (order new-term))
	  (new-term-coeff-poly (coeff new-term)))
      (define (iter poly-termlist new-coeff-termlist)
	(if (empty-termlist? poly-termlist)
	    new-coeff-termlist
	    (cond
	     ((= (order (first-term poly-termlist)) term-order)
	      (adjoin-term (make-term 
			    (order poly-term) 
			    (coeff (first-term poly-termlist)))
			   new-coeff-termlist))
	     (else (iter (rest-terms poly-termlist) new-coeff-termlist)))))
      (make-term term-order 
		 (make-polynomial
		  (variable (contents new-term-coeff-poly))
		  (iter (term-list (contents (coeff poly-term)))
			(term-list (contents new-term-coeff-poly)))))))


  (define (add-to-coefficient-from-constant
	   new-variable
	   term
	   first-term)
    (if (= 0 (order term)) 
	(make-term
	 (order term)
	 (make-polynomial
	  (variable (contents (coeff term)))
	  (adjoin-term
	   (make-term (order first-term)
		      (coeff first-term))
	   (term-list (contents (coeff term))))))
	term))

  ;; transform poly to be in terms of new-variable.
  (define (make-term-of-order-in new-variable term-order poly)
    (define (should-be-constant? a-term)
      (let ((poly (coeff a-term)))
	(and (= (term-list-length (term-list (contents poly))) 1)
	     (= (order (first-term (term-list (contents poly)))) 0))))

    (define (constant-coefficient a-term)
      (coeff (first-term (term-list (contents (coeff a-term))))))

    (define (iter termlist term)
      (if (empty-termlist? termlist)
	  term
	  (let ((first-term (first-term termlist))
		(rest-terms (rest-terms termlist)))
	    (if (polynomial-in? new-variable (contents (coeff first-term)))
		(iter rest-terms (add-to-coefficient-from-poly
				  new-variable
				  term
				  first-term))
		(iter rest-terms (add-to-coefficient-from-constant
				  new-variable
				  term
				  first-term))))))
    (let ((constructed-term (iter (term-list poly) 
				  (make-term 
				   term-order 
				   (make-polynomial (variable poly) (the-empty-termlist))))))
      (cond
       ((should-be-constant? constructed-term)
	(make-term term-order (constant-coefficient constructed-term)))
       (else constructed-term))))

  (define (polynomial-in? var poly)
    (and (pair? poly)
	 (same-variable? var (variable poly))))


  (define (transform-terms new-variable poly)
    (define (iter termlist orders)
      (if (null? orders)
	  termlist
	  (let ((new-term (make-term-of-order-in new-variable (car orders) poly)))
	    (iter (adjoin-term new-term
			       termlist)
		  (cdr orders)))))
    (make-polynomial new-variable (iter (the-empty-termlist) 
				  (list-orders-of-coefficients-in new-variable
								  poly))))

  (define (empty-set) '())
  (define (adjoin-to-set x set)
    (cond ((or (null? set) (< x (car set))) (cons x set))
	  ((= x (car set)) set)
	  (else (cons (car set) (adjoin-to-set x (cdr set))))))
  (define (union set1 set2)
    (cond
     ((null? set1) set2)
     ((null? set2) set1)
     (else
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond
	 ((= x1 x2) (cons x1 (union (cdr set1) (cdr set2))))
	 ((< x1 x2) (cons x1 (union (cdr set1) set2)))
	 ((< x2 x1) (cons x2 (union set1 (cdr set2)))))))))

  (define (set->list set) set)

  ;; list of orders of coefficients that are polynomials in terms of variable
  ;; for polynomial, which is in terms of some other variable.
  (define (list-orders-of-coefficients-in variable polynomial)
    (define (iter order-set termlist)
      (if (empty-termlist? termlist)
	  order-set
	  (let ((found-orders (find-set-of-orders variable
						  (first-term termlist))))
	    (iter (union found-orders order-set) (rest-terms termlist)))))
    (set->list (iter (empty-set) (term-list polynomial))))

  (define (find-set-of-orders var poly-term)
    (define (iter-terms set-of-terms a-term-list)
      (if (empty-termlist? a-term-list)
	  set-of-terms
	  (iter-terms
	   (adjoin-to-set (order (first-term a-term-list)) set-of-terms)
	   (rest-terms a-term-list))))

    (if (polynomial-in? var (contents (coeff poly-term)))
	(iter-terms (empty-set) (term-list (contents (coeff poly-term))))
	(adjoin-to-set 0 (empty-set))))

  
  (define (highest-order-of var poly)
    (define (highest-term-of-list term-list)
      (cond
       ((empty-termlist? (rest-terms term-list)) (order (first-term term-list)))
       (else
	(highest-term-of-list (rest-terms term-list)))))

    (define (highest-order-term var poly)
      (if (same-variable? (variable poly) var)
	  (highest-term-of-list (term-list poly))
	  false))
    

    (define (iter-poly-list highest termlist)
      (if (empty-termlist? termlist) 
	  highest
	  (let ((this-order (highest-order-term var (coeff (first-term termlist)))))
	    (if (and this-order (> this-order highest))
		(iter-poly-list this-order (rest-terms termlist))
		(iter-poly-list highest (rest-terms termlist))))))
    (iter-poly-list -1 (term-list poly)))

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
  (define (div-poly p1 p2)
    (if
     (same-variable? (variable p1) (variable p2))
     (make-poly (variable p1)
		(div-terms (term-list p1) (term-list p2)))
     (error
      "These polys cannot be divided, they are not in the same variable -- DIV-POLY"
      p1 p2)))
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
       (lambda (p1 p2)
	 (tag
	  (let ((coerced (coerce-to-highest-variable-pair p1 p2)))
	    (add-poly (car coerced) (cdr coerced))))))
  (put 'add '(polynomial integer)
       (lambda (p1 i)
	 (tag (make-poly (variable p1) (adjoin-term (make-term 0 i) (term-list p1))))))

  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
  (put 'div-poly '(polynomial polynomial)
       (lambda (p1 p2)
	 (let ((result (div-poly p1 p2)))
	   (let ((quotient (car result))
		 (remainder (cadr result)))
	     (list (tag quotient) (tag remainder))))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
	 (tag (car (div-poly p1 p2)))))
  'done)

(define (div-poly p1 p2)
  ((get 'div-poly '(polynomial polynomial)) (contents p1)  (contents p2)))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
