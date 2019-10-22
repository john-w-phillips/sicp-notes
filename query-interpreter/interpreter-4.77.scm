(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define put-hash (make-hash-table))
(define (put key ns value)
  (let ((ns-table (hash-table-ref put-hash ns (lambda () #f))))
    (if ns-table
	(hash-table-set! ns-table key value)
	(let ((new-hash (make-hash-table)))
	  (hash-table-set! put-hash ns new-hash)
	  (hash-table-set! new-hash key value)))))

(define (get key ns)
  (let ((ns-table (hash-table-ref put-hash ns (lambda () #f))))
    (if ns-table
	(hash-table-ref ns-table key (lambda () #f))
	#f)))
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
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define (tagged-list? obj symb)
  (and (pair? obj) (eq? (car obj) symb)))
(define (display-stream a-stream)
  (if (not (stream-null? a-stream))
      (begin
	(user-print (stream-car a-stream))
	(newline)
	(flush-output)
	(display-stream (stream-cdr a-stream)))))

      
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
	   (add-rule-or-assertion! (add-assertion-body q))
	   (newline)
	   (display "Assertion added to data base.")
	   (query-driver-loop))
	  (else
	   (newline)
	   (announce-output output-prompt)
	   (display-stream
	    (stream-map
	     (lambda (frame)
	       (instantiate
		   q
		   frame
		 (lambda (v f)
		   (contract-question-mark v))))
	     (apply-frame-filters
	      (qeval false q (singleton-stream the-empty-frame)))))
	   (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
	   (let ((binding (binding-in-frame exp frame)))
	     (if binding
		 (copy (binding-value binding))
		 (unbound-var-handler exp frame))))
	  ((pair? exp)
	   (cons (copy (car exp)) (copy (cdr exp))))
	  (else exp)))
  (copy exp))

(define (unbound-variables? exp frame)
  (define (walk exp)
    (cond ((var? exp)
	   (let ((binding (binding-in-frame exp frame)))
	     (if binding
		 (walk (binding-value binding))
		 true)))
	  ((pair? exp)
	   (or (walk (car exp)) (walk (cdr exp))))
	  (else
	   false)))
  (walk exp))

(define (qeval rulename query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
	(qproc rulename (contents query) frame-stream)
	(simple-query query frame-stream))))

(define (apply-frame-filters frame-stream)
  (stream-filter
   (lambda (frame)
     (not (eq? frame 'failed)))
   (stream-map
    (lambda (frame)
      (apply-filters frame))
    frame-stream)))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (extend-if-not-failed
	 var
	 val
	 frame)
  (if (eq? frame 'failed)
      'failed
      (extend var val frame)))


(define (unify-frames-with-variables
	 bindings-list
	 higher-frame
	 lower-frame)
  (cond
   ((null? bindings-list) the-empty-frame)
   (else
    (let* ((rest-frame-unified
	    (unify-frames-with-variables
	     (cdr bindings-list)
	     higher-frame
	     lower-frame)))
      (cond ((eq? 'failed rest-frame-unified) 'failed)
	    (else
	     (let ((unified (unify-binding (car bindings-list)
					   higher-frame
					   lower-frame)))
	       (cond
		((eq? 'failed unified) 'failed)
		((eq? 'unbound unified) rest-frame-unified)
		(else
		 (begin
		   (extend (car bindings-list)
			   unified
			   rest-frame-unified)))))))))))

(define (get-variable-list frame)
  (map binding-variable (frame-bindings frame)))

(define (unique a-list)
  (define (unique-iter output-list a-list)
    (cond ((null? a-list)
	   output-list)
	  ((member (car a-list) output-list)
	   (unique-iter output-list (cdr a-list)))
	  (else
	   (unique-iter (cons (car a-list) output-list)
			(cdr a-list)))))
  (unique-iter '() a-list))
	
(define (unify-frames
	 frame1
	 frame2)
  (let ((varslist (unique (append
			   (get-variable-list frame1)
			   (get-variable-list frame2))))
	(filterlist (append (frame-filters frame1)
			    (frame-filters frame2))))
    (let ((frame-withbindings
	   (unify-frames-with-variables
	    varslist
	    frame1
	    frame2)))
      (if (not (eq? frame-withbindings 'failed))
	  (make-frame
	   (frame-bindings frame-withbindings)
	   filterlist)
	  'failed))))
		       

(define (print-if-middle-manager string binding)
  (if (and (pair? binding) (not (null? (member 'middle-manager binding))))
      (begin
	(display string)
	(display binding)
	(newline))
      '()))
(define (unify-binding
	 variable
	 higher-frame
	 lower-frame)
  (let* ((b1 (binding-in-frame variable higher-frame))
	 (b2 (binding-in-frame variable lower-frame)))
    (cond
     ((and (false? b1) (false? b2)) (display "UNBOUND\n") 'unbound)
     ((false? b1)
      (binding-value b2))
     ((false? b2)
      (binding-value b1))
     ((equal?  b1 b2)
      (binding-value b1))
     ((and (var? (binding-value b1))
	   (not (var? (binding-value b2))))
      (check-binding
       (binding-value b1)
       (binding-value b2)
       higher-frame))
     ((and (var? (binding-value b2))
	   (not (var? (binding-value b1))))
      (check-binding
       (binding-value b2)
       (binding-value b1)
       lower-frame))
     ((and (var? (binding-value b1))
	   (var? (binding-value b2)))
      (follow-binding-chain
       b1
       b2
       higher-frame
       lower-frame))

     (else 'failed))))

(define (follow-binding-chain
	 b1
	 b2
	 higher-frame
	 lower-frame)
  (cond
   ((and (false? b1) (false? b2)) false)
   ((and (false? b1) (not (false? b2))) (binding-value b2))
   ((and (false? b2) (not (false? b1))) (binding-value b1))
   ((and (var? (binding-value b1))
	 (var? (binding-value b2)))
    (let ((followed (follow-binding-chain (binding-in-frame (binding-value b1) higher-frame)
					  (binding-in-frame (binding-value b2) lower-frame)
					  higher-frame
					  lower-frame)))
      (if (false? followed)
	  (binding-value b2)
	  followed)))
   ((and (var? (binding-value b1))
	 (not (var? (binding-value b2))))
    (check-binding
     (binding-value b2)
     b1
     higher-frame))
   ((and (var? (binding-value b2))
	 (not (var? (binding-value b1))))
    (check-binding
     (binding-value b1)
     b2
     lower-frame))
   ((equal? (binding-value b1) (binding-value b2))
    (binding-value b2))
   (else 'failed)))


(define (check-binding found-value binding-for-chain-variable frame)
  (cond
   ((false? binding-for-chain-variable) found-value)
   ((var? (binding-value binding-for-chain-variable))
    (let ((binding (binding-in-frame
		    (binding-value binding-for-chain-variable frame))))
      (if binding
	  (check-binding found-value binding frame)
	  found-value)))
   ((equal? (binding-value binding-for-chain-variable)
	    found-value)
    found-value)
   (else
    'failed)))
	  
			 
;; (define (extend-bindings
;; 	 b1
;; 	 b2
;; 	 frame)
;;   (let* ((var (binding-variable b1))
;; 	 (val (binding-value b1))
;; 	 (var2 (binding-variable b2))
;; 	 (val2 (binding-value b2)))
;;     (cond
;;      ((and (not (var? val))
;; 	   (not (var? val2))
;; 	   (equal? val val2))
;;       (extend var val frame))
;;      ((and (var? val2) (not (var? val)))
;;       (extend-if-not-failed var2 val2
;; 			    (extend-if-binding-possible
;; 			     b1
;; 			     (binding-in-frame val2 frame)
;; 			     frame)))
;;      ((and (var? val) (not (var? val2)))
;;       (extend-if-not-failed var val
;; 			    (extend-if-not-failed
;; 			     (binding-in-frame val frame)
;; 			     b2
;; 			     frame)))
;;      ((and (var? val) (var? val2))
;;       (let ((binding-1 (binding-in-frame val frame))
;; 	    (binding-2 (binding-in-frame val2 frame)))
;; 	(cond
;; 	 ((and (false? binding-1) (false? binding-2))
;; 	  (extend-if-not-failed
;; 	   var val
;; 	   (extend-if-not-failed
;; 	    var2 val2 frame)))
;; 	 ((false? binding-2)
;; 	  (extend-if-not-failed val2 (binding-value binding-1)
;; 				frame))
;; 	 ((false? binding-1)
;; 	  (extend-if-not-failed val
;; 				(binding-value binding-2)
;; 				frame))
;; 	 (else
;; 	  (extend-if-not-failed var val
;; 				(extend-if-not-failed
;; 				 var2
;; 				 val2
;; 				 (extend-if-binding-possible
;; 				  (binding-in-frame val frame)
;; 			      (binding-in-frame val2 frame)
;; 			      frame)))))))
;;      (else 'failed))))

;; (define (extend-if-binding-possible
;; 	 b1
;; 	 b2
;; 	 frame)
;;   (cond ((false? b1) 'failed)
;; 	((eq? frame 'failed) 'failed)
;; 	((false? b2)
;; 	 (extend (binding-variable b1)
;; 		 (binding-value b1)
;; 		 frame))
;; 	(else (extend-bindings b1 b2 frame))))

;; (define (bindings-unify
;; 	 frame1
;; 	 frame2)
;;   (cond ((null? frame1) frame2)
;; 	((null? frame2) frame1)
;; 	(else
;; 	 (let* ((b1 (first-binding frame1))
;; 		(b2 (binding-in-frame (binding-variable b1) frame2))
;; 		(rest-unified
;; 		  (bindings-unify
;; 		   (rest-bindings frame1)
;; 		   (remove-binding (binding-variable b1) frame2))))
;; 	     (extend-if-binding-possible
;; 	      b1
;; 	      b2
;; 	      rest-unified)))))



(define (join-frames stream1 stream2)
  (cond ((empty-stream? stream1)
	 the-empty-stream)
	((empty-stream? stream2)
	 the-empty-stream)
	(else
	 (stream-filter (lambda (f)
			  (not (eq? f 'failed)))
			(interleave-delayed
			 (stream-map (lambda (f)
				       (unify-frames
					(stream-car stream1)
					f))
				     stream2)
			(delay (join-frames (stream-cdr stream1) stream2)))))))

(define (dependent-clause? current-rulename a-clause)
  (or (tagged-list? a-clause 'lisp-value)
      (tagged-list? a-clause 'not)
      (tagged-list? a-clause current-rulename)))

(define (condense-frames listof-frames)
  (if (null? (cdr listof-frames))
      (car listof-frames)
      (join-frames (car listof-frames)
		   (condense-frames (cdr listof-frames)))))

(define (conjoin rulename conjuncts frame-stream)
  (define (conjoin-iter conjuncts current-frame-stream frame-streams)
    (cond
     ((empty-conjunction? conjuncts) (condense-frames (reverse frame-streams)))
     ((dependent-clause? rulename (first-conjunct conjuncts))
      (let ((eval-result (qeval
			  rulename
			  (first-conjunct conjuncts) 
			  (condense-frames (reverse frame-streams)))))
	(conjoin-iter (rest-conjuncts conjuncts)
		      eval-result
		      (list eval-result))))
     (else
      (conjoin-iter (rest-conjuncts conjuncts)
		    current-frame-stream
		    (cons (qeval rulename
				 (first-conjunct conjuncts)
				 current-frame-stream)
			  frame-streams)))))
  (conjoin-iter conjuncts frame-stream (list frame-stream)))
	

;; (define (conjoin conjuncts frame-stream)
;;   (if (empty-conjunction? conjuncts)
;;       the-empty-stream
;;       (join-frames
;;        (qeval (first-conjunct conjuncts) frame-stream)
;;        (conjoin (rest-conjuncts conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

(define (disjoin rulename disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval rulename (first-disjunct disjuncts) frame-stream)
       (delay (disjoin rulename (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

(define (negate rulename operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (cond
      ((unbound-variables? (negated-query operands) frame)
       (singleton-stream (attach-filter frame
					(lambda (frame2)
					  (if (stream-null?
					       (qeval rulename
						      (negated-query operands)
						      (singleton-stream frame2)))

						frame2
						'failed)))))
      ((stream-null?
	  (qeval rulename
		 (negated-query operands)
		 (singleton-stream frame)))
       (singleton-stream frame))
      (else the-empty-stream)))
   frame-stream))
(put 'not 'qeval negate)

(define (unique-body call) (car call))

(define (uniquely-asserted rulename call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((evaled (qeval
		    rulename
		    (unique-body call)
		    (singleton-stream frame))))
       (if (stream-null? (stream-cdr evaled))
	   evaled
	   the-empty-stream)))
   frame-stream))
(put 'unique 'qeval uniquely-asserted)

(define (lisp-value rulename call frame-stream)
  (let ((execution-proc
	 (lambda (newframe)
	   (if (execute (instantiate
			    call
			    newframe
			  (lambda (v f) "Unknown pat var: LISP-VALUE" v)))
	       newframe
	       'failed))))
    (stream-flatmap
     (lambda (frame)
       (cond
	((unbound-variables? call frame)
	 (singleton-stream (attach-filter frame
					  execution-proc)))
	((not (eq? 'failed (execution-proc frame)))
	 (singleton-stream frame))
	(else
	 the-empty-stream)))
     frame-stream)))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
	 (args exp)))


(define (always-true ignore1 ignore2 frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum)
     (check-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-assertion assertion query-pat query-frame)
  (let ((match-result
	 (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
	the-empty-stream
	(singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
	((equal? pat dat) frame)
	((var? pat) (extend-if-consistent pat dat frame))
	((and (pair? pat) (pair? dat))
	 (pattern-match
	  (cdr pat)
	  (cdr dat)
	  (pattern-match (car pat) (car dat) frame)))
	(else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
	(pattern-match (binding-value binding) dat frame)
	(extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
		    (apply-a-rule rule pattern frame))
		  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
				     (conclusion clean-rule)
				     query-frame)))
      (if (eq? unify-result 'failed)
	  the-empty-stream
	  (qeval
	   (rule-name rule)
	   (rule-body clean-rule)
	   (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
	     (make-new-variable exp rule-application-id))
	    ((pair? exp)
	     (cons (tree-walk (car exp))
		   (tree-walk (cdr exp))))
	    (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
	((equal? p1 p2) frame)
	((var? p1) (extend-if-possible p1 p2 frame))
	((var? p2) (extend-if-possible p2 p1 frame))
	((and (pair? p1) (pair? p2))
	 (unify-match (cdr p1)
		      (cdr p2)
		      (unify-match (car p1)
				   (car p2)
				   frame)))
	(else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
	   (unify-match (binding-value binding) val frame))
	  ((var? val)
	   (let ((binding (binding-in-frame val frame)))
	     (if binding
		 (unify-match
		  var (binding-value binding) frame)
		 (extend var val frame))))
	  ((depends-on? val var frame)
	   'failed)
	  (else (extend var val frame)))))


(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
	   (if (equal? var e)
	       true
	       (let ((b (binding-in-frame e frame)))
		 (if b
		     (tree-walk (binding-value b))
		     false))))
	  ((pair? e)
	   (or (tree-walk (car e))
	       (tree-walk (cdr e))))
	  (else false)))
  (tree-walk exp))

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
      (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))


(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
	(let ((current-assertion-stream
	       (get-stream key 'assertion-stream)))
	  (put key
	       'assertion-stream
	       (cons-stream
		assertion
		current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
	(let ((key (index-key-of pattern)))
	  (let ((current-rule-stream
		 (get-stream key 'rule-stream)))
	    (put key
		 'rule-stream
		 (cons-stream rule
			      current-rule-stream)))))))
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
	(stream-cdr s1)
	delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
	(force delayed-s2)
	(delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp) (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))
(define (rule-name rule)
  (caadr rule))
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))


(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
	 (cons (map-over-symbols proc (car exp))
	       (map-over-symbols proc (cdr exp))))
	((symbol? exp) (proc exp))
	(else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
	(list '?
	      (string->symbol
	       (substring chars 1 (string-length chars))))
	symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
		  (if (number? (cadr variable))
		      (string-append (symbol->string (caddr variable))
				     "-"
				     (number->string (cadr variable)))
		      (symbol->string (cadr variable))))))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame)))

(define (make-frame bindings filters)
  (list bindings filters))
(define (frame-bindings frame)
  (car frame))
(define (frame-filters frame)
  (cadr frame))

(define the-empty-frame (make-frame '() '()))


;; (define (remove-binding variable frame)
;;   (filter (lambda (binding)
;; 	    (not (equal? (binding-variable binding) variable)))
;; 	  frame))

(define (first-binding bindings-list)
  (car (frame-bindings frame)))
(define (rest-bindings frame)
  (cdr frame))

(define (attach-filter frame filter)
  (make-frame
   (frame-bindings frame)
   (cons filter
	 (frame-filters frame))))

(define (apply-filters frame)
  (fold-right
   (lambda (new-filter previous)
     (if (not (eq? previous 'failed))
	 (new-filter previous)
	 'failed))
   (make-frame (frame-bindings frame) '())
   (frame-filters frame)))
   
(define (extend variable value frame)
  (make-frame
   (cons (make-binding variable value)
	 (frame-bindings frame))
   (frame-filters frame)))
	 
