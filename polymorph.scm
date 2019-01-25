(define *op-table* (make-hash-table))
(define *coercion-table* (make-hash-table))


(define (put op type item)
  (cond
   ((hash-table-exists? *op-table* op)
    (hash-table-set! (hash-table-ref *op-table* op) type item))
   (else
    (begin
      (let ((new-table (make-hash-table)))
	(hash-table-set! new-table type item)
	(hash-table-set! *op-table* op new-table))))))
(define (get op type)
  (cond
   ((not (hash-table-exists? *op-table* op)) false)
   (else
    (let ((table-for-op (hash-table-ref *op-table* op)))
      (if
       (not (hash-table-exists? table-for-op type))
       false
       (hash-table-ref table-for-op type))))))

(define (put-coercion in-type out-type proc)
  (hash-table-set! *coercion-table* (list in-type out-type) proc))

(define (get-coercion in-type out-type)
  (cond
   ((not (hash-table-exists? *coercion-table* (list in-type out-type)))
    false)
   (else
    (hash-table-ref *coercion-table* (list in-type out-type)))))

(define (get-coercions-list)
  (let ((alist (hash-table->alist *coercion-table*)))
    (map (lambda (item)
	   (list (caar item)
		 (cadar item)
		 (cdr item)))
	 alist)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (find-matching-coercion op args)))))

;; find-matching-coercion -- find a coercion that can be used on the arguments
;; to get a procedure using get, and apply it to the coerced arguments.
(define (find-matching-coercion op args)
  (let ((coercion-typetag-list (get-listof-possible-coercions (map type-tag args))))
    (let ((found-types-and-proc (find-working-typetag-list op coercion-typetag-list args)))
      (if found-types-and-proc
	  (apply (car found-types-and-proc) (map contents (cdr found-types-and-proc)))
	  (error "Unable to find coercion -- FIND-MATCHING-COERCION" op args)))))

(define (flatmap proc elems)
  (cond
   ((null? elems) '())
   (else
    (append (proc (car elems))
	    (flatmap proc (cdr elems))))))

(define (get-listof-possible-coercions listof-typetags)
  (cond
   ((null? listof-typetags) '(()))
   (else
    (let ((rest-possible-coercions
	   (get-listof-possible-coercions (cdr listof-typetags)))
	  (coercion-destinations-for-this-type
	   (get-listof-coerced-types (car listof-typetags))))
      (flatmap
       (lambda (first)
	 (map
	  (lambda (rest) (cons first rest))
	  rest-possible-coercions))
       (cons (car listof-typetags) coercion-destinations-for-this-type))))))

(define (get-listof-coerced-types a-type)
  (let ((coercions (get-coercions-list)))
    (map (lambda (coercion) (cadr coercion)) 
	 (filter (lambda (coercion) (eq? (car coercion) a-type)) coercions))))

(define (find-working-typetag-list op listof-possible-typetags original-args-list)
  (cond
   ((null? listof-possible-typetags) false)
   (else
    (let ((coercions (map (lambda (sym1 sym2) 
			    (if (eq? sym1 sym2)
				false
				(get-coercion sym1 sym2)))
			  (map type-tag original-args-list)
			  (car listof-possible-typetags))))
      (let ((coerced (map (lambda (proc arg) (if proc (proc arg) arg))
			  coercions
			  original-args-list)))
	(let ((proc (get op (car listof-possible-typetags))))
	  (if proc
	      (cons proc coerced)
	      (find-working-typetag-list
	       op
	       (cdr listof-possible-typetags)
	       original-args-list))))))))

(define (attach-tag tag datum)
  (cond
   ((eq? 'scheme-number tag) datum)
   (else (cons tag datum))))

(define (type-tag datum)
  (cond
   ((number? datum) 'scheme-number)
   (else (car datum))))

(define (contents datum)
  (cond
   ((number? datum) datum)
   (else (cdr datum))))


  
