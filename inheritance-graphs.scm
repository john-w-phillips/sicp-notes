(define (edge-pairs-to-towers listof-edge-pairs)
    (define (iter-sequences listof-sequences)
      (if (null? listof-sequences)
	  '()
      (let ((merged-list (find-a-merge
                 		   (car listof-sequences)
			           (cdr listof-sequences))))
	   (if merged-list
    	       (iter-sequences merged-list)
	       (cons (car listof-sequences)
		     (iter-sequences (cdr listof-sequences)))))))
    (define (find-a-merge an-item the-rest)
      (cond
       ((null? the-rest) false)
       ((eq? (cadr (last an-item)) (caaar the-rest))
	(cons (append an-item (car the-rest)) (cdr the-rest)))
       ((eq? (caar an-item) (cadr (last (car the-rest))))
	(cons
	 (append (car the-rest) an-item)
	 (cdr the-rest)))
       (else
	(let ((rest (find-a-merge an-item (cdr the-rest))))
	  (if rest
	      (cons (car the-rest) rest)
	      false)))))

    (iter-sequences (map list listof-edge-pairs)))

(define (sorted-pairs->list sorted-pairs)
  (cond
   ((null? sorted-pairs)
    (error "Must have a list of at least length one -- SORTED-PAIRS"))
   ((null? (cdr sorted-pairs))
    (list (caar sorted-pairs) (cadar sorted-pairs)))
   (else
    (cons (caar sorted-pairs) (sorted-pairs->list (cdr sorted-pairs))))))	  

(define (make-table-into-towers listof-pairs)
  (map (lambda (x)
	 (reverse (sorted-pairs->list x)))
       (edge-pairs-to-towers listof-pairs)))
  
