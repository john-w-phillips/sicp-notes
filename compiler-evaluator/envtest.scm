(define language-apply apply)
(define (rest-items lists)
  (cond
   ((null? lists) '())
   (else
    (cons (cdr (car lists))
	  (rest-items (cdr lists))))))

(define (first-items lists)
  (cond
   ((null? lists) '())
   (else
    (cons (caar lists)
	  (first-items (cdr lists))))))

(define (all-null? lists)
  (cond
   ((null? lists) true)
   (else
    (and (null? (car lists))
	 (all-null? (cdr lists))))))

(define (map aproc . lists)
  (cond
   ((all-null? lists) '())
   (else
    (cons (language-apply aproc (first-items lists))
	  (language-apply map (append (list aproc) (rest-items lists)))))))

(define (filter aproc list)
  (if (null? list)
      '()
      (if (aproc (car list))
	  (cons (car list) (filter aproc (cdr list)))
	  (filter aproc (cdr list)))))

(define (append2 list1 list2)
  (cond
   ((null? list1) list2)
   (else (cons (car list1)
	       (append2 (cdr list1)
			list2)))))

(define (append . lists)
  (cond
   ((null? lists) '())
   ((null? (cdr lists)) (car lists))
   (else
    (append2 (car lists)
	     (language-apply append (cdr lists))))))


(define (id x) x)
(define (square x) (* x x))
(define (simple-map proc alist)
  (cond ((null? alist) '())
	(else (cons (proc (car alist))
		    (simple-map proc (cdr alist))))))

;;(language-apply append (list '(a b c d e) '(1 2 3 4 5)))
(append '(a b c d e) '(1 2 3 4 5))
(language-apply simple-map (list (lambda (x) (* x x)) '(1 2 3 4 5)))
(filter (lambda (x) (= x 0)) (list 1 0 1 0 1 0))

