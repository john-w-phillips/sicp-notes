(load "baselib.scm")
(assert (= (let ((x 10) (y 20) (z 2)) (* (+ x y) z)) 60))
(define (f x) (or (= x 3) (= x 5)))
(assert
 (eq? 'third
      (cond ((f 1) 'first)
	    ((f 2) 'second)
	    ((f 3) 'third)
	    (else 'fail))))
;; (assert
;;  (eq? 'fail
;;       (cond
;;        ((f 1) 'first)
;;        ((f 2) 'second)
;;        (else 'fail))))
