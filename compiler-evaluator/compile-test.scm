(define (map aproc alist)
  (if (null? alist)
      '()
      (cons (aproc (car alist))
	    (map aproc (cdr alist)))))

(define (f alist)
  (cons 'ex (map (lambda (num) (* num 2)) alist)))
(f (list 1 2 3 45))

