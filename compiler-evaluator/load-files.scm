(define (load-inst? expr) (and (pair? expr) (eq? (car expr) 'load)))
(define (load-file-name expr) (cadr expr))

(define (load-file filename)
  (let* ((exprs '())
	 (inport (open-input-file filename)))
    (define (load-file-iter exprs)
      (let ((next-read (read inport)))
	(if (eof-object? next-read)
	    exprs
	    (if (load-inst? next-read)
		(let ((loaded (load-file (load-file-name next-read))))
		  (load-file-iter
		   (append exprs loaded)))
		(load-file-iter
		 (append exprs (list next-read)))))))
    (let ((results
	   (load-file-iter exprs)))
      (close-port inport)
      results)))

(define (file-as-sequence filename)
  (let ((exprs (load-file filename)))
    (cons 'begin exprs)))
    
    
