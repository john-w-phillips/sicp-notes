(assert! (rule (mcr ?x ?y ?z)
	       (and
		(p1 ?x ?y)
		(p2 ?y ?z)
		(p3 ?x ?z))))
(assert! (p1 (f p) q))
(assert! (p2 m n))
(assert! (p1 t u))
(assert! (p3 l m))
(assert! (p2 q m))
(assert! (p3 (f p) q))
(assert! (p3 p m))
(assert! (p3 (f p) m))