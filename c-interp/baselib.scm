(define (null? x) (eq? x '()))

(define defmacro (macro (args . body)
		   `(define ,(car args)
		      (macro ,(cdr args)
			,@body))))

(define (map proc alist)
  (if (null? alist) '()
      (cons (proc (car alist))
	    (map proc (cdr alist)))))


(defmacro (let bindings . body)
  `((lambda ,(map (lambda (x) (car x)) bindings)
      ,@body)
    ,@(map (lambda (x) (car (cdr x))) bindings)))


(define (cond->if condform)
  (if (null? condform)
      '()
      (if (eq? 'else (car (car condform)))
	  (cons 'begin (cdr (car condform)))
	  `(if ,(car (car condform))
	       (begin
		 ,@(cdr (car condform)))
	       ,(cond->if (cdr condform))))))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(defmacro (cond . body)
  (cond->if body))

(define (filter proc alist)
  (cond ((null? alist) '())
	((proc (car alist))
	 (cons (car alist)
	       (filter proc (cdr alist))))
	(else (filter proc (cdr alist)))))

(defmacro (and . body)
  (if (null? body) 'true
      `(if (not ,(car body))
	   false
	   ,(cons 'and (cdr body)))))

(defmacro (or . body)
  (if (null? body) 'false
      `(if ,(car body)
	   true
	   ,(cons 'or (cdr body)))))
	   

