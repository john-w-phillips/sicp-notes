(define (filter aproc list)
  (if (null? list)
      '()
      (if (aproc (car list))
	  (cons (car list) (filter aproc (cdr list)))
	  (filter aproc (cdr list)))))

(filter (lambda (x) (= x 1)) (list 1 0 1 1 0 0))
