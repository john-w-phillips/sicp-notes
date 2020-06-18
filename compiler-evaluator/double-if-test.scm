(define (f n)
  (if (and (> n 3) (< n 6)) true))

(if (f 5)
    (if (f 4)
	(if (f 3)
	    1
	    0)
	-1)
    -2)
