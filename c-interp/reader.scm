(load "baselib.scm")
(define (scheme-stdin-reader)
  (let ((infd 0)
	(ungets '()))
    (list
     (lambda ()
       (if (eq? ungets '())
	   (vector-ref (cdr (sys-read infd 1)) 0)
	   (begin
	     (let ((rv (car ungets)))
	       (set! ungets (cdr ungets))
	       rv))))
     (lambda (c)
       (set! ungets (cons c ungets)))
     (lambda (c)
       0))))

	
(define (scheme-file-reader fd)
  (let ((infd fd)
	(ungets '()))
    (list
     (lambda ()
       (if (eq? ungets '())
	   (let ((readreturn (sys-read infd 1)))
	     (if (<= (car readreturn) 0)
		 eof
		 (vector-ref (cdr readreturn) 0)))
	   (begin
	     (let ((rv (car ungets)))
	       (set! ungets (cdr ungets))
	       rv))))
     (lambda (c)
       (set! ungets (cons c ungets)))
     (lambda (c)
       0))))

(define (make-reader-for-filename fname)
  (let ((fd (sys-open fname "r")))
    (scheme-file-reader fd)))


(define (read-scheme-file fname)
  (define (read-aux rd forms)
    (let ((form (read rd)))
      (if (eq? form eof)
	  (cons 'begin (reverse forms))
	  (read-aux rd (cons form forms)))))
  (let ((rd (make-reader-for-filename fname)))
    (read-aux rd '())))
    
