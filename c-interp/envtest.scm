(assert
 (= ((lambda (x)
       ((lambda (y)
	  ((lambda (x)
	     (+ (* x y) (- x 2)))
	   (+ x y 3)))
	(+ x 2)))
     5)
    118))
    


