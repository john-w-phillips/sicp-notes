(load-option 'format)
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define put-hash (make-hash-table))
(define the-empty-stream '())


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
  (when (not (stream-null? a-stream))
      (begin
	(user-print (stream-car a-stream))
	(newline)
	(flush-output)
	(display-stream (stream-cdr a-stream)))))

      
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ;; ((assertion-to-be-added? q)
	  ;;  (add-rule-or-assertion! (add-assertion-body q))
	  ;;  (newline)
	  ;;  (display "assertion added to data base.")
	  ;;  (query-driver-loop))
	  (else
	   (newline)
	   (announce-output output-prompt)
	   (display-stream
	    (stream-map
	     (lambda (frame)
	       (%instantiate
		   q
		   frame
		 (lambda (v f)
		   (contract-question-mark v))))
	     (qeval false q (singleton-stream 
			     (extend-environment
			      the-empty-frame
			      the-global-environment)))))
	   (query-driver-loop)))))

(define (%instantiate exp environment unbound-var-handler)
  (define (copy exp env)
    (cond ((var? exp)
	   (let ((splice (splice-environment-on-binding exp env)))
	     (if splice
		 (copy (binding-value (splice-binding splice))
		       (parent-environment (splice-lower splice)))
		 (unbound-var-handler exp env))))
	  ((pair? exp)
	   (cons (copy (car exp) env) (copy (cdr exp) env)))
	  (else exp)))
  (copy exp environment))

(define (qeval rulename query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
	(qproc rulename (contents query) frame-stream)
	(simple-query query frame-stream))))

(define (simple-query query-pattern environment-stream)
    (stream-flatmap
     (lambda (environment)
       (stream-append-delayed
	(find-assertions query-pattern environment)
	(delay (apply-rules query-pattern environment))))
     environment-stream))

	

(define (conjoin rulename conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin
       rulename
       (rest-conjuncts conjuncts)
       (qeval rulename (first-conjunct conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

(define (query-debug rulename ignore1 ignore2)
  (debug)
  the-empty-stream)
(put 'debug 'qeval query-debug)

(define (store-rule cur-rulename new-rule-expr environment-stream)
  (let ((actual-expr (car new-rule-expr)))
    (begin
      (if (rule? actual-expr)
	  (stream-map
	   (lambda (environment)
	     (add-rule-binding-to-frame!
	      actual-expr
	      (first-frame environment))
	     environment)
	   environment-stream)
	  (add-assertion! actual-expr))
      the-empty-stream)))
(put 'assert! 'qeval store-rule)

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
     (let ((qeval-result (qeval rulename
				(negated-query operands)
				(singleton-stream frame))))
       (if (stream-null?
	    qeval-result)
	   (singleton-stream frame)
	   the-empty-stream)))
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
  (stream-flatmap
   (lambda (frame)
     (if (execute
	  (%instantiate
	      call
	      frame
	    (lambda (v f)
	      (error "unkown pat var: lisp-value" v))))
	 (singleton-stream frame)
	 the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
	 (args exp)))


(define (always-true ignore1 ignore2 frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern environment)
  (stream-flatmap
   (lambda (datum)
     (check-assertion datum pattern environment))
   (fetch-assertions pattern environment)))

(define (check-assertion assertion query-pat query-environment)
  (let ((match-result
	 (pattern-match query-pat
			assertion
			query-environment)))
    (if (eq? match-result 'failed)
	the-empty-stream
	(singleton-stream match-result))))

(define (pattern-match pat dat query-environment)
  (cond ((eq? query-environment 'failed)
	 (trace-fail 'pattern-match "Recursive fail" query-environment))
	((equal? pat dat) query-environment)
	((var? dat) (error "Var is dat"))
	((var? pat) (extend-if-consistent pat dat query-environment))
	((and (pair? pat) (pair? dat))
	 (pattern-match
	  (cdr pat)
	  (cdr dat)
	  (pattern-match (car pat) (car dat) query-environment)))
	(else (trace-fail 'pattern-match
			  (format #f "Pattern: ~a and data: ~a do not match"
				  pat
				  dat)
			  query-environment))))


(define (extend-if-consistent var dat query-environment)
  (let ((splice
	 (splice-environment-on-binding var query-environment)))
    (if splice
	(let ((patmatch
	       (pattern-match (binding-value (splice-binding splice))
			      dat
			      (parent-environment (splice-lower splice)))))
	  (if (eq? patmatch 'failed)
	      'failed
	      (concat-splice
	       (splice-upper splice)
	       (extend-environment
		(first-frame (splice-lower splice))
		patmatch))))
	  (extend-environment
	   (extend-frame var dat (first-frame query-environment))
	   (parent-environment query-environment)))))

(define (apply-rules pattern environment)
  (stream-flatmap (lambda (rule)
		    (apply-a-rule rule pattern environment))
		  (fetch-rules pattern environment)))
(define *is-tracing-on* false)
(define (set-tracing) (set! *is-tracing-on* true) 'tracing-on)
(define (stop-tracing) (set! *is-tracing-on* false) 'tracing-off)
(define (trace-fail identifier message environment)
  (if *is-tracing-on*
    (begin
      (format #t "Failing from ~a message: ~a~%" identifier message)
      (format #t " ENV: ~a~%" environment)
      'failed)
    'failed))

(define (scan-rule-for-vars rule-body frame)
  (cond
   ((eq? frame 'failed) 'failed)
   ((null? rule-body) frame)
   ((var? rule-body)
    (if (or (binding-in-frame rule-body frame)
	    (bound-in-parents? rule-body frame))
	frame
	(add-declared rule-body frame)))
   ((pair? rule-body)
    (scan-rule-for-vars
     (car rule-body)
     (scan-rule-for-vars
      (cdr rule-body)
      frame)))
   (else frame)))

(define (add-declared var frame)
  (extend-parent-binding-in-frame #f var frame))
(define (scan-for-local-rules rule env)
  (let ((rule-body-forms (cddr rule)))
    (define (scan-body body)
      (if (null? body) (list env body)
	  (let ((form-tag (caar body)))
	    (if (eq? form-tag 'assert!)
		(begin (qeval
			(rule-name rule)
			(car body)
			(singleton-stream env))
		       (scan-body (cdr body)))
		(list env (car body))))))
    (scan-body rule-body-forms)))

	  
(define (apply-a-rule rule query-pattern environment)
  (let ((new-env (unify-match query-pattern
			      (conclusion rule)
			      (extend-environment the-empty-frame environment))))
    (let* ((scanned-env
	   (if (eq? new-env 'failed) 'failed
	       (extend-environment
		(scan-rule-for-vars (rule-body rule)
				    (first-frame new-env))
		(parent-environment new-env))))
	  (env-with-rules-and-body
	   (if (eq? scanned-env 'failed) 'failed
	       (scan-for-local-rules rule scanned-env))))
      (if (eq? env-with-rules-and-body 'failed)
	  the-empty-stream
	  (stream-map
	   (lambda (environ)
	     (collapse-environment-by-one environ))
	   (qeval
	    (rule-name rule)
	    (cadr env-with-rules-and-body)
	    (singleton-stream (car env-with-rules-and-body))))))))

;; (define (apply-a-rule rule query-pattern query-frame)
;;   (let ((clean-rule (rename-variables-in rule)))
;;     (let ((unify-result (unify-match query-pattern
;; 				     (conclusion clean-rule)
;; 				     query-frame)))
;;       (if (eq? unify-result 'failed)
;; 	  the-empty-stream
;; 	  (qeval
;; 	   (rule-name rule)
;; 	   (rule-body clean-rule)
;; 	   (singleton-stream unify-result))))))

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

(define (map-to-child parent-pat child-pat)
  (cond ((and (null? parent-pat)
	      (null? child-pat)) '())
	((and (pair? parent-pat)
	      (pair? child-pat))
	 (map-to-child (cdr parent-pat)
		       (cdr child-pat)
		       (map-to-child
			(car parent-pat)
			(car child-pat))))
	(else 'failed)))

#| We assume that the result of unify match is: 
 a simple, two-part mapping. That is to say that the child section
maps variables to either literal values or expressions that contain
parent variables. The parent variable expressions will, on a pattern
match, need to be 'resolved'. So a challenge left is to modify
pattern-match to work.
|#
(define (unify-match parent-pat child-pat env)
  (cond ((eq? env 'failed) (trace-fail 'unify-match "Recursive fail case" env))
	((and (null? parent-pat) (null? child-pat)) env)
	((var? parent-pat) (extend-parent-binding parent-pat child-pat env))
	((var? child-pat) (extend-child-binding child-pat parent-pat env))
	((and (pair? parent-pat) (pair? child-pat))
	 (unify-match (cdr parent-pat)
		      (cdr child-pat)
		      (unify-match (car parent-pat)
				   (car child-pat)
				   env)))
	((equal? parent-pat child-pat) env)
	(else (trace-fail 'unify-match "Bad child/parent pair" env))))

(define (extend-child-binding child-pat parent-pat env)
  (let ((binding (binding-in-frame child-pat (first-frame env))))
    (cond (binding
	   (unify-match parent-pat (binding-value binding) env))
	  (else (extend-environment
		 (extend-frame child-pat parent-pat
			       (first-frame env))
		 (parent-environment env))))))


(define (extend-parent-binding parent-pat child-pat env)
  (let ((binding (binding-in-environment parent-pat env)))
    (cond (binding
	   (unify-match (binding-value binding) child-pat env))
	  (else (extend-environment
		 (extend-parent-binding-in-frame
		  parent-pat
		  child-pat
		  (first-frame env))
		 (parent-environment env))))))

(define (extend-if-possible var val
			    frame)
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

(define the-assertions the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) the-assertions)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define the-rules the-empty-stream)
(define (fetch-rules pattern environment)
  (if (pair? pattern)
      (fetch-all-rules (car pattern) environment)
      the-empty-stream))

(define (fetch-all-rules name environment)
  (cond
   ((null? environment) the-empty-stream)
   (else
    (let ((assoc-result (assoc name (environment-frame-rule-bindings
				     (first-frame environment)))))
      (if assoc-result
	  (cons-stream
	   (cdr assoc-result)
	   (fetch-all-rules name (parent-environment environment)))
	  (fetch-all-rules name (parent-environment environment)))))))

(define (get-all-rules) the-rules)
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
  (let ((old-assertions the-assertions))
    (set! the-assertions
      (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules the-rules))
    (set! the-rules (cons-stream rule old-rules))
    'ok))


(define (store-assertion-in-index assertion)
  (when (indexable? assertion)
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
    (when (indexable? pattern)
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
      (error "unknown expression type" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "unknown expression contents" exp)))

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



(define the-empty-environment '())
(define (environment-frame? e)
  (= (length e) 3))

(define (extend-environment env-frame env)
  (cond ((not (environment-frame? env-frame))
	 (error "Invalid frame" env-frame))
	((not (or (pair? env) (null? env)))
	 (error "Invalid environment" env))
	(else
	 (cons env-frame env))))

(define (bound-in-parents? variable frame)
  (if (not (null? (filter
		   (lambda (cell)
		     (equal? (binding-value cell) variable))
		   (environment-frame-parent-mappings frame))))
      true
      false))

(define (splice-environment-on-binding variable environment)
  (define (iter current-upper-part current-lower-part)
    (if (null? current-lower-part)
	false
	(let ((binding (binding-in-frame variable (first-frame current-lower-part))))
	  (cond
	   (binding (list binding current-upper-part current-lower-part))
	   ((bound-in-parents? variable (first-frame current-lower-part))
	    ;; if the variable is pointed to by a parent 'binding' but not
	    ;; actually bound yet, it should be bound at some point, and 
	    ;; should not be considered 'bound' if a parent frame has it.
	    false)
	   (else
	    (iter (extend-environment (first-frame current-lower-part) current-upper-part)
		  (parent-environment current-lower-part)))))))
  (iter '() environment))

(define (splice-binding splice)
  (car splice))
(define (splice-upper splice)
  (cadr splice))
(define (splice-lower splice)
  (caddr splice))
(define (concat-splice upper lower)
  (append upper lower))

(define (binding-in-environment variable environment)
  (if (null? environment)
      false
      (let ((binding (binding-in-frame variable (car environment))))
	(cond
	 (binding
	  binding)
	 ((bound-in-parents? variable (first-frame environment))
	  false)
	 (else (binding-in-environment variable (cdr environment)))))))

(define (parent-environment env)
  (cdr env))
(define (first-frame env)
  (let ((thecar (car env)))
    (if (environment-frame? thecar)
	thecar
	(error "Not an environment frame -- FIRST-FRAME" env))))

(define (make-environment-frame parent-variable-mappings
				variable-bindings
				procedure-bindings)
  (list
   parent-variable-mappings
   variable-bindings
   procedure-bindings))
(define the-empty-frame (make-environment-frame '() '() '()))
(define the-empty-environment (list the-empty-frame))
(define the-global-environment (list the-empty-frame))
(define (extend-frame var val frame)
  (make-environment-frame
   (environment-frame-parent-mappings frame)
   (cons (make-binding var val)
	 (environment-frame-bindings frame))
   (environment-frame-rule-bindings frame)))

(define (extend-frame-with-rule rule-expr frame)
  (make-environment-frame
   (environment-frame-parent-mappings frame)
   (environment-frame-bindings frame)
   (cons rule-expr (environment-frame-rule-bindings frame))))

(define (add-rule-binding-to-frame! binding-form frame)
  (set-car! (cddr frame) (cons (cons (rule-name binding-form) binding-form)
			       (caddr frame))))
(define (rule-binding-in-frame rule-name frame)
  (assoc rule-name frame))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (rule-binding-in-frame rulename frame)
  (assoc rulename frame))
 
(define (binding-in-frame variable frame)
  (assoc variable (environment-frame-bindings frame)))

(define (parent-binding-in-frame variable frame)
  (assoc variable (environment-frame-parent-mappings frame)))
(define (binding-from-bindings variable bindings)
  (assoc variable bindings))

(define (environment-frame-parent-mappings frame)
  (car frame))
(define (environment-frame-bindings frame)
  (cadr frame))

(define (environment-frame-rule-bindings frame)
  (caddr frame))

;; (define (extend-parent-binding
;; 	 variable
;; 	 child-binding
;; 	 child-bindings
;; 	 parent-bindings)
;;   (cond
;;    ((false? child-binding) (error "bad child binding"))
;;    ((var? (binding-value child-binding))
;;     (extend-parent-binding
;;      variable
;;      (binding-in-frame
;;       (binding-value child-binding)
;;       child-bindings)
;;      child-bindings
;;      parent-bindings))
;;    (else
;;     (extend variable
;; 	    (binding-value child-binding)
;; 	    parent-bindings))))

(define (extend-parent-binding-in-frame
	 variable
	 value
	 frame)
  (make-environment-frame
   (cons (make-binding variable value)
	 (environment-frame-parent-mappings frame))
   (environment-frame-bindings frame)
   (environment-frame-rule-bindings frame)))

(define (extend-bindings var val bindings)
  (cons (cons var val) bindings))

;; Take a 'lower' frame from a rule application
;; or pattern match, and merge it up into the parent frame.
;;
;; This involves:
;; 1. eliminating any local bindings except
;; 2. Any bindings in the lower frame parent mappings,
;;    These bindings are in the form (parent-variable -> child-variable),
;;    i.e. parent bound to child var. Any variable found in this form
;;    will be 'collapsed', the parent-variable binding will take on the value
;;    of the child-variable binding.
;;
;; The reutrned frame has the same local bindings as the parent (env-to-instantiate),
;; except ones bound transitively in the child through parent bindings.
(define (instantiate-environment-frame
	 env-to-instantiate
	 env-instantiate-from)
  (define (iterate-mappings child-mappings parent-bindings)
    (if (null? child-mappings)
	parent-bindings
	(let ((first-child-binding (first-binding child-mappings)))
	  (let ((parent-binding (binding-from-bindings
				 (binding-variable
				  first-child-binding)
				 parent-bindings)))
	    (if parent-binding
		;; If the parent has a binding, it 'shadows' the child binding.
		(iterate-mappings (rest-bindings child-mappings)
				  parent-bindings)
		(let ((child-variable-binding
			(binding-from-bindings 
			 (binding-value first-child-binding)
			 (environment-frame-bindings
			  env-instantiate-from))))
		  (if child-variable-binding
		      (iterate-mappings (rest-bindings child-mappings)
					(extend-bindings
					 (binding-variable first-child-binding)
					 (binding-value child-variable-binding)
					 parent-bindings))
		      (iterate-mappings (rest-bindings child-mappings)
					parent-bindings))))))))

  (let ((mappings (environment-frame-parent-mappings env-instantiate-from)))
    (let ((parent-bindings (environment-frame-bindings
			    env-to-instantiate)))
      (make-environment-frame
       (environment-frame-parent-mappings env-to-instantiate)
       (iterate-mappings mappings parent-bindings)
       (environment-frame-rule-bindings env-to-instantiate)))))

(define (collapse-environment-by-one
	 environment)
  (if (or (null? environment) (null? (cdr environment)))
      environment
      (let ((first (car environment))
	    (second (cadr environment))
	    (rest (cddr environment)))
	(extend-environment
	 (instantiate-environment-frame
	  second
	  first)
	 rest))))
;; (define (remove-binding variable frame)
;;   (filter (lambda (binding)
;; 	    (not (equal? (binding-variable binding) variable)))
;; 	  frame))

(define (first-binding bindings-list)
  (car bindings-list))
(define (rest-bindings frame)
  (cdr frame))
