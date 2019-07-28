(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (cond
   ((lazycons-cell? object)
    (display (lazylist->schemelist object)))
   ((compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>)))
   (else 
   (display object))))

