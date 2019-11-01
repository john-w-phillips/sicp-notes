;; copy and paste this into query prompt.
(assert! (rule (myrec ?x ?y)
	       (or (p1 ?x ?y)
		   (and (p1 ?x ?intermediate)
			(myrec ?intermediate ?y)))))
(assert! (p1 a b))
(assert! (p1 b c))
(assert! (p1 c d))
(assert! (p1 d e))
