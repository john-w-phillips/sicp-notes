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
	   (vector-ref (cdr (sys-read infd 1)) 0)
	   (begin
	     (let ((rv (car ungets)))
	       (set! ungets (cdr ungets))
	       rv))))
     (lambda (c)
       (set! ungets (cons c ungets)))
     (lambda (c)
       0))))  
