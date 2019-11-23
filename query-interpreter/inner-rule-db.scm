(assert! (p1 (thing one)))
(assert! (p2 (thing two)))
(assert! (p2 (thing one)))
(assert! (p1 (thing three)))
(assert! (p2 (thing four)))
(assert! (p1 (thing five)))
(assert! (p1 (thing six)))
(assert! (p1 (one two) (two three)))

 ;; A simple application of nested rule.
(assert! (rule (simple-rule ?x ?y ?z)
	       ;; assert! is necessary, otherwise it thinks this is a pattern
	       ;; match.
		(assert! (rule (helper-rule ?r ?y)
			       (and (p1 ?r) (p2 ?y))))
		(and (helper-rule ?x ?y)
		     (helper-rule ?y ?z)
		     (helper-rule ?x ?z))))

(assert! (rule (complicated-rule ?x ?y)
	       (assert! (p-inner (one two)))
	       (assert! (p-inner (two three)))
	       (assert! (rule (helper-rule ?x ?y)
			      (and (p-inner ?x)
				   (p-inner ?y)
				   (p1 ?x ?y))))
	       (helper-rule ?x ?y)))

;; (and (and (p1 ?x) (p2 ?y)) (and (p1 ?y) (p2 ?z)) (and (p1 ?x) (p2 ?z)))
