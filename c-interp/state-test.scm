(load "baselib.scm")
(define g (let ((x 1) (y 0))
	    (list
	     (lambda (c) (set! x c) c)
	     (lambda (c) (set! y c) c)
	     (lambda () x)
	     (lambda () y))))
((car g) 3)
((car (cdr g)) 4)
(load "fibotest.scm")
(fibo 20)
(fibo 20)
(assert (= ((car (cdr (cdr g)))) 3))
(assert (= ((car (cdr (cdr (cdr g))))) 4))


