(define (fibo n)
  (if (< n 2) n
      (+ (fibo (- n 1)) (fibo (- n 2)))))
(assert (= (fibo 20) 6765))

