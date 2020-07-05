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

(define vec-mixed (make-vector 'vector-mixed 'a 'b 'c '(1 2 3)))
(assert (equal? (vector-ref vec-mixed 3) '(1 2 3)))

(assert (vector=? "abc" "abc"))
(assert (vector=? (make-vector 'vector-mixed 'a 'b '(1 2 3))
		  (make-vector 'vector-mixed 'a 'b '(1 2 3))))
(assert (not (vector=? (make-vector 'vector-mixed 'a 'b 'c)
		       (make-vector 'vector-mixed 'a 'b 1))))

(assert (vector=? "abc" (vector-concat "a" (vector-concat "b" "c"))))


(assert (eq? (string-ref "abc" 0) ?a))
(assert (eq? (string-ref "abc" 1) ?b))
