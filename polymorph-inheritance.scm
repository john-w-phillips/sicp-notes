(load "inheritance-graphs.scm")
(define *op-table* (make-hash-table))
(define *coercion-table* (make-hash-table))
(define *inheritance-graph* '())
(define *projection-table* (make-hash-table))

(define (projectable? x)
  (not (false? (get-projection x))))

(define (drop x)
  (let ((x-tag (type-tag x)))
    (cond
     ((and (projectable? (type-tag x)) ((get 'equ? (list x-tag x-tag))
					(contents x) (contents (raise (project x)))))
      (drop (project x)))
     (else x))))

  
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

(define (put-projection t1 proc)
  (hash-table-set! *projection-table*
		   t1
		   proc))

(define (get-projection t1)
  (cond
   ((not (hash-table-exists? *projection-table* t1))
    false)
   (else
    (hash-table-ref *projection-table* t1))))

(define (project x)
  ((get-projection (type-tag x)) x))

(define (put-raise t1 t2 proc)
  (put 'raise (list t1) proc)
  (put-coercion t1 t2 proc)
  (put-inheritance t1 t2))

(define (put-inheritance t1 t2)
  (set! *inheritance-graph* (cons (list t1 t2) *inheritance-graph*)))

(define (get-parent t1)
  (define (search-list inlist)
    (cond
     ((null? inlist) #f)
     ((eq? (caar inlist) t1)
      (cadar inlist))
     (else
      (search-list (cdr inlist)))))
  (search-list *inheritance-graph*))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc 
	  (let ((val (apply proc (map contents args))))
	    (if (type-in-system? val)
		(drop val)
		val))
	  (let ((raised-list (raise-args op args)))
	    (apply apply-generic (cons op raised-list)))))))

(define (raise x)
  ((get-coercion
    (type-tag x)
    (get-parent (type-tag x))) x))

(define (raise-args op args)
  (cond
   ((all-same-type? args)
    (raise-all-or-fail args))
   ((not (args-in-same-heirarchy? args))
    (error
     (string-append
      "Can't apply any function to these arguments."
      "They are not in the same type heirarchy and can't be coerced via "
      "inheritance to any other type -- RAISE-ARGS")
     args))
   (else
    (let ((highest-type-tag (get-highest-type args)))
      (map (lambda (arg) (raise-to highest-type-tag arg))
	   args)))))

(define (args-in-same-heirarchy? args)
  (let ((type-towers (map get-type-tower-for args)))
    (not (false? (reduce-right
		  (lambda (x r) (if (equal? x r) r false))
		  '()
		  type-towers)))))

(define (raise-all-or-fail args)
  (map (lambda (x) (if (raiseable? x) (raise x)
		       (error "can't raise object to another type RAISE-ALL-OR-FAIL"
			      x
			      args)))
       args))

(define (raiseable? x)
  (let ((type-tower (get-type-tower-for (type-tag x)))
	(tag (type-tag x)))
    (and (not (false? (memq tag type-tower)))
	 (not (eq? tag (car type-tower))))))

(define (all-same-type? args)
  (let ((types (map type-tag args)))
    (let ((first (car types))
	  (rest (cdr types)))
    (not (false? (fold (lambda (a r)
			 (if (eq? a r)
			     a
			     #f))
		       first
		       rest))))))
 
(define (get-highest-type objects)
  (define (iter-highest highest objs)
    (cond
     ((null? objs) highest)
     ((or (higher? highest (car objs))
	  (eq? highest (car objs)))
      (iter-highest highest (cdr objs)))
     ((higher? (car objs) highest)
      (iter-highest (car objs) (cdr objs)))
     (else (error
	    "No matching procedure for types, they are not in the same type hierarchy"
	    objects))))
  (iter-highest (type-tag (car objects)) (map type-tag (cdr objects))))

(define (raise-to a-type-tag object)
  (cond
   ((eq? (type-tag object) a-type-tag)
    object)
   (else (raise-to a-type-tag (raise object)))))

(define (higher? type1 type2)
  (let ((type-map1 (get-type-tower-for type1))
	(type-map2 (get-type-tower-for type2)))
    (if (not (equal? type-map1 type-map2))
	false
	(let ((type1-in-list (memq type1 type-map1))
	      (type2-in-list (memq type2 type-map1)))
	  (if (and type1-in-list type2-in-list)
	      (not (memq type1 type2-in-list))
	      false)))))

(define (get-type-towers)
    (make-table-into-towers
     *inheritance-graph*))

(define (get-type-tower-for x)
  (let ((filtered
	 (filter (lambda (tower)
		   (memq x tower))
		 (get-type-towers))))
    (if (not (null? filtered))
	(car filtered)
	'())))
 
(define (attach-tag tag datum)
  (cond
   ((or
     (eq? 'integer tag)
     (eq? 'real tag)) datum)
   (else (cons tag datum))))

(define (type-tag datum)
  (cond
   ((and (number? datum)
	 (integer? datum)
	 (exact? datum)) 'integer)
   ((and (number? datum) (inexact? datum)) 'real)
   (else (car datum))))

(define (contents datum)
  (cond
   ((number? datum) datum)
   (else (cdr datum))))

(define (type-in-system? datum)
  (fold-right
   (lambda (x r) (or (not (false? x))
		      r))
   false
   (map (lambda (tower) (memq datum tower))
	(get-type-towers))))
