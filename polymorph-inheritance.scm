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
  ((get-coercion (type-tag x) (get-parent (type-tag x))) x))

(define (raise-args op args)
  (cond
   ((all-same-type? args)
    (raise-all-or-fail args))
   (else
    (let ((highest-type-tag (get-highest-type args)))
      (map (lambda (arg) (raise-to highest-type-tag arg))
	   args)))))

(define (raise-all-or-fail args)
  (map (lambda (x) (if (raiseable? x) (raise x)
		       (error "can't raise object to another type RAISE-ALL-OR-FAIL"
			      x
			      args)))
       args))

(define (raiseable? x)
  (let ((type-tower (get-type-tower))
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
  (let ((type-map (get-type-tower)))
    (let ((type1-in-list (memq type1 type-map))
	  (type2-in-list (memq type2 type-map)))
      (if (and type1-in-list type2-in-list)
	  (not (memq type1 type2-in-list))
	  false))))

(define (unsorted-pairs-to-chain listof-pairs)
  (define (insertable? element resultlist)
    (cond
     ((null? resultlist) false)
     ((eq? (cadr element) (caar resultlist))
      true)
     ((eq? (car element) (cadar resultlist))
      true)
     (else (insertable? element (cdr resultlist)))))

  (define (insert-one-element elem listof-pairs)
    (cond
     ((null? listof-pairs) (list elem))
     ((eq? (cadr elem) (caar listof-pairs))
      (cons elem listof-pairs))
     ((eq? (car elem) (cadar listof-pairs))
      (cons (car listof-pairs) (cons elem (cdr listof-pairs))))
     (else (cons (car listof-pairs) (insert-one-element elem (cdr listof-pairs))))))

  (define (find-insertable listof-pairs result)
    (cond
     ((null? result) listof-pairs)
     ((null? listof-pairs) 
      (error "Can't insert any element from this into that -- FIND-INSERTABLE"
	     listof-pairs result))
     (else
      (let ((first (car listof-pairs)))
	(if (insertable? first result)
	    listof-pairs
	    (let ((insertable-list (find-insertable (cdr listof-pairs) result)))
	      (cons (car insertable-list) 
		    (cons first (cdr insertable-list)))))))))

  (define (iter-pairs result listof-pairs)
    (cond 
     ((null? listof-pairs) result)
     (else
      (let ((insertable-first (find-insertable listof-pairs result)))
	(iter-pairs (insert-one-element (car insertable-first) result)
		    (cdr insertable-first))))))
  (iter-pairs '() listof-pairs))

(define (sorted-pairs->list sorted-pairs)
  (cond
   ((null? sorted-pairs)
    (error "Must have a list of at least length one -- SORTED-PAIRS"))
   ((null? (cdr sorted-pairs))
    (list (caar sorted-pairs) (cadar sorted-pairs)))
   (else
    (cons (caar sorted-pairs) (sorted-pairs->list (cdr sorted-pairs))))))

(define (make-table-into-tower listof-pairs)
  (reverse (sorted-pairs->list (unsorted-pairs-to-chain listof-pairs))))

(define (get-type-tower)
    (make-table-into-tower
     *inheritance-graph*))
 
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
  (cond
   ((or
     (number? datum)
     (and (pair? datum)
	  (not (false? (memq (car datum) (get-type-tower))))))
    true)
   (else
    false)))
