;; Testing for exercise 2.92, polynomial transformation. Moving it
;; outside the type tagging system for now.
(define VAR-ORDER
  '(x y z t u v w a b c p q))

(define (make-term order coeff)
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (=zero? x)
  (or (and (number? x) (= 0 x))
      (and (pair? x) (=poly-zero? x))))

(define (same-variable? x y)
  (and (symbol? x) (symbol? y) (eq? x y)))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (term-list-length term-list) (length term-list))
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

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

;; is v1 higher than v2?
(define (higher-variable? v1 v2)
  (null? (memq v1 (memq v2 VAR-ORDER))))

(define (coerce-to-variable variable poly)
  (transform-terms variable poly))

(define (coerce-to-highest-variable-pair poly1 poly2)
  (if (same-variable? (variable poly1) (variable poly2))
      (cons poly1 poly2)
      (cond
       ((higher-variable? poly1)
	(cons poly1 (coerce-to-variable (variable poly1) poly2)))
       (else (cons (coerce-to-variable (variable poly2) poly1) poly2)))))

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
	       (make-poly 
		(variable new-term-coeff-poly) 
		(iter (term-list (coeff poly-term))
		      (term-list new-term-coeff-poly))))))

(define (add-to-coefficient-from-constant
	 new-variable
	 term
	 first-term)
  (if (= 0 (order term)) 
      (make-term
       (order term)
       (make-poly
	(variable (coeff term))
	(adjoin-term
	 (make-term (order first-term)
		    (coeff first-term))
	 (term-list (coeff term)))))
      term))

;; transform poly to be in terms of new-variable.
(define (make-term-of-order-in new-variable term-order poly)
  (define (should-be-constant? a-term)
    (let ((poly (coeff a-term)))
      (and (= (term-list-length (term-list poly)) 1)
	   (= (order (first-term (term-list poly))) 0))))

  (define (constant-coefficient a-term)
    (coeff (first-term (term-list (coeff a-term)))))

  (define (iter termlist term)
    (if (empty-termlist? termlist)
	term
	(let ((first-term (first-term termlist))
	      (rest-terms (rest-terms termlist)))
          (if (polynomial-in? new-variable (coeff first-term))
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
				 (make-poly (variable poly) (the-empty-termlist))))))
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
  (make-poly new-variable (iter (the-empty-termlist) 
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
	 (adjoin-to-set (order (first a-term-list)) set-of-terms)
	 (rest-terms a-term-list))))

  (if (polynomial-in? var (coeff poly-term))
      (iter-terms (empty-set) (term-list (coeff poly-term)))
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
