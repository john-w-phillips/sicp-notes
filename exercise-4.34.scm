(define (require x) (if (not x) (amb)))

(define (make-statement person place)
  (list person place))
(define (statement-person a-statement) (car a-statement))
(define (statement-place a-statement) (cadr a-statement))
(define betty-statements (list
			  (make-statement 'betty 3)
			  (make-statement 'kitty 2)))
(define ethel-statements (list
			  (make-statement 'ethel 1)
			  (make-statement 'joan 2)))
(define joan-statements (list
			 (make-statement 'joan 3)
			 (make-statement 'ethel 5)))
(define kitty-statements (list
			  (make-statement 'kitty 2)
			  (make-statement 'mary 4)))
(define mary-statements (list
			 (make-statement 'betty 1)
			 (make-statement 'mary 4)))

(define (make-statement-with-truth statement truthvalue)
  (cons statement truthvalue))
(define (statement-with-truth-statement statement-with-truth)
  (car statement-with-truth))
(define (statement-with-truth-truth statement-with-truth)
  (cdr statement-with-truth))
(define (statement-with-truth-person statement-with-truth)
  (statement-person (statement-with-truth-statement statement-with-truth)))
(define (statement-with-truth-place statement-with-truth)
  (statement-place (statement-with-truth-statement statement-with-truth)))
(define (one-amb-statement a-statement)
  (amb (list (make-statement-with-truth (car a-statement) true)
	     (make-statement-with-truth (cadr a-statement) false))
       (list (make-statement-with-truth (car a-statement) false)
	     (make-statement-with-truth (cadr a-statement) true))))
(define (generate-statement-set statements)
  (if (null? statements)
      '()
      (append (one-amb-statement (car statements)) 
	      (generate-statement-set (cdr statements)))))

(define (get-true-statements statements)
  (if (null? statements)
      '()
      (if (statement-with-truth-truth (car statements))
	  (cons (car statements) (get-true-statements (cdr statements)))
	  (get-true-statements (cdr statements)))))

(define (require-no-same-place statements)
  (define (is-other-in-same-place? statement statements)
    (cond
     ((null? statements) false)
     ((and (= (statement-with-truth-place statement)
	      (statement-with-truth-place (car statements)))
	   (not (eq? (statement-with-truth-person statement)
		     (statement-with-truth-person (car statements)))))
      true)
     (else (is-other-in-same-place? statement (cdr statements)))))
  (let ((true-statements (get-true-statements statements)))
    (define (iter-statements current-statement-list)
      (if (null? current-statement-list) true
	  (begin
	    (require (not (is-other-in-same-place? (car current-statement-list)
						   (cdr current-statement-list))))
	    (iter-statements (cdr current-statement-list)))))
    (iter-statements true-statements)))

(define (require-no-true-and-false statements)
  (define (require-no-duplicates a-statement statement-list)
    (if (null? statement-list)
	true
	(let ((statement-from-list (car statement-list)))
	  (require (or
		    (not (eq? (statement-with-truth-person statement-from-list)
			      (statement-with-truth-person a-statement)))
		    (and (=
			  (statement-with-truth-place a-statement)
			  (statement-with-truth-place statement-from-list))
			 (eq?
			  (statement-with-truth-truth a-statement)
			  (statement-with-truth-truth statement-from-list)))
		    (and (false? (statement-with-truth-truth a-statement))
			 (false? (statement-with-truth-truth statement-from-list)))
		    (and (not (= (statement-with-truth-place a-statement)
				 (statement-with-truth-place statement-from-list)))
			 (not (eq?
			       (statement-with-truth-truth a-statement)
			       (statement-with-truth-truth statement-from-list))))))
	  (require-no-duplicates a-statement (cdr statement-list)))))
  (if (null? statements)
      true
      (begin
	(and (require-no-duplicates (car statements) (cdr statements))
	     (require-no-true-and-false (cdr statements))))))

(define (require-no-contradictions statements)
  (require-no-same-place statements)
  (require-no-true-and-false statements)
  statements)

(define all-statements (list betty-statements
			     ethel-statements
			     joan-statements
			     kitty-statements
			     mary-statements))
(define (get-accurate-statements)
  (let ((statement-set (generate-statement-set all-statements)))
    (require-no-contradictions statement-set)))

(define (sub1 x) (- x 1))
(define (require-placings-accurate placing-list statement-list)
  (define (iterate-statements statements)
    (if (null? statements)
	true
	(begin
	  (let ((statement (car statements)))
	    (let ((placement
		   (list-ref placing-list (sub1 (statement-with-truth-place statement)))))
	     (if (statement-with-truth-truth statement)
		 (require (eq? placement (statement-with-truth-person statement)))
		 (require (not (eq? placement (statement-with-truth-person statement)))))))
	   (iterate-statements (cdr statements)))))
  (iterate-statements statement-list))


(define (require-placings-different placing-list)
  (define (require-single-placing-different
	   a-placing
	   placings)
    (if (null? placings)
	true
	(begin
	  (require
	   (not (eq? a-placing (car placings))))
	  (require-single-placing-different
	   a-placing
	   (cdr placings)))))
  (if (null? placing-list)
      true
      (begin
	(require-single-placing-different
	 (car placing-list)
	 (cdr placing-list))
	(require-placings-different (cdr placing-list)))))

(define (get-placings)
  (let ((statements (get-accurate-statements))
	(placings (list (amb 'ethel 'joan 'kitty 'mary 'betty)
			(amb 'ethel 'joan 'kitty 'mary 'betty)
			(amb 'ethel 'joan 'kitty 'mary 'betty)
			(amb 'ethel 'joan 'kitty 'mary 'betty)
			(amb 'ethel 'joan 'kitty 'mary 'betty))))
    (require-placings-different placings)
    (require-placings-accurate placings statements)
    placings))
