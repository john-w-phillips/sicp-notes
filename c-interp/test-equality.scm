(assert (eq? 'symbol1 'symbol1))
(assert (eq? 'symbol2 'symbol2))
(assert (eq? '() '()))
(assert (eq? true true))
(assert (eq? false false))
(assert (not (eq? true false)))
(assert (pair? '(1 2 3)))
(assert (not (pair? 1)))

