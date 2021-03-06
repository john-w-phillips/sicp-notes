(load "../eceval-impl/environment.scm")
;; Given address, which is a list (<frames> <offset)
;; of the item to access, return a variable.
(define (lexical-address-lookup address env)
  (let ((frame-offset (car address))
	(var-offset (cadr address)))
    (if (< (length env) frame-offset)
	(error "Invalid address, environment not long enough -- LEXICAL-ADDRESS-LOOKUP")
	(let ((frame (list-ref env frame-offset)))
	  (let ((var (list-ref (frame-values frame) var-offset)))
	    (if (eq? var UNDEFINED-VARIABLE)
		(error "Unassigned variable -- LEXICAL-ADDRESS-LOOKUP" address)
		var))))))

(define (lexical-address-set! address env value)
  (let ((frame-offset (car address))
	(var-offset (cadr address)))
    (if (< (length env) frame-offset)
	(error "Invalid offset, environment not long enough -- LEXICAL-ADDRESS-SET!")
	(let ((frame (list-ref env frame-offset)))
	  (let ((varcel (list-ref (frame-variable-value-pairs frame) var-offset)))
	    (set-cdr! varcel value)
	    'ok)))))

(define the-empty-compile-time-env '())

(define (compile-time-env-extend varlist env)
  (cons varlist env))

(define (find-variable var compenv)
  (define (find-variable-frame-iter i frame)
    (cond
     ((null? frame) false)
     ((eq? (car frame) var)
      i)
     (else (find-variable-frame-iter (+ i 1) (cdr frame)))))
  (define (find-variable-env-iter i env)
    (if (null? env)
	false
	(let ((search-result
	       (find-variable-frame-iter 0 (car env))))
	  (if (not (false? search-result))
	      (list i search-result)
	      (find-variable-env-iter (+ 1 i) (cdr env))))))
  (find-variable-env-iter 0 compenv))
