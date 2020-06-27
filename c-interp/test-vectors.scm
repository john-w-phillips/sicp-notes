(load "baselib.scm")
(load "fibotest.scm")
(let ((vec (make-vector 'int 1 2 3 4 5 6)))
  (assert (= (vector-ref vec 0) 1))
  (assert (= (vector-ref vec 5) 6))
  (vector-set! vec 5 7)
  (assert (= (vector-ref vec 5) 7)))

(define vecsave (make-vector 'int 1 2 3 4 5 6 7 8 9 10 11 12))
;; trigger GC several times, to make sure the vector doesn't get obliterated.
(fibo 20)
(fibo 20)
(assert (= (vector-ref vecsave 11) 12))
