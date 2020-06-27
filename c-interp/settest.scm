(define x 3)
(assert (= x 3))
(set! x 4)
(assert (= x 4))
(assert
 (= (begin
      (set! x 2)
      (assert (= x 2))
      (set! x 5)
      (assert (= x 5))
      x)
    5))
  
