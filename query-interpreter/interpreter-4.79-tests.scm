(load "interpreter-4.79.scm")

(let ((result (unify-match '((f (y (? x))) (g (m (? x))) (? z))
			   '((? x) (? y) (? z)) the-empty-frame)))
  (if
      (and
       (equal? (parent-binding-in-frame '(? x) result)
	       '(f (y (? x))))
       (equal? (parent-binding-in-frame '(? y) result)
	       '(g (m (? x)))))
      (display "pass\n")
      (display "fail\n")))
