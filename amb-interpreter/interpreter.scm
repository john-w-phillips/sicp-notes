(define language-apply apply)
;; (load "eval-apply.scm")
(load "analyze.scm")

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
	  (try-again)
	  (begin
	    (newline) (display ";;; Starting a new problem ")
	    (eval
	     input
	     the-global-environment
	     ;; ambeval success
	     (lambda (val next-alternative)
	       (announce-output output-prompt)
	       (user-print val)
	       (internal-loop next-alternative))
	     ;; ambeval failure
	     (lambda ()
	       (announce-output
		";;; There are no more values of")
	       (user-print input)
	       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(driver-loop)
	  
