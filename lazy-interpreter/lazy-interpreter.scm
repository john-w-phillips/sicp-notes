(load "interpreter-prompt.scm")
(load "lazy-eval.scm")
(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (primitive-load filename)
  (let ((file (open-input-file filename)))
    (let reader ((obj (read file)))
	(if (not (eof-object? obj))
	    (begin
	      (eval obj the-global-environment)
	      (reader (read file)))
	    'ok))))

(define the-global-environment
  (extend-environment
   (list
    'load
    'car
    'cons
    'cdr)
   (list (make-primitive-procedure primitive-load)
	 (make-lazy-primitive-procedure lazycar)
	 (make-lazy-primitive-procedure lazycons)
	 (make-lazy-primitive-procedure lazycdr))
   the-global-environment))

(primitive-load "base-code.tscm")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
	   (actual-value
	    input
	    the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(driver-loop)
