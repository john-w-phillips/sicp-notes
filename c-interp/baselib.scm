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
;; FOrm:
;; (define-struct abc
;;  (slot-a slot-b slot-c))
(define (slots-to-pairs listofslots)
  (define (slots-to-pair-iter i slots rv)
    (if (null? slots) rv
	(slots-to-pair-iter
	 (+ i 1)
	 (cdr slots)
	 (cons (cons (car slots) i)
	       rv))))
  (slots-to-pair-iter 0 listofslots '()))
(define string-concat vector-concat)
(define string-append vector-concat)

(defmacro (define-struct name slots)
  (let ((namestr (symbol->string name))
	(slotpairs (slots-to-pairs slots)))
  `(begin
     (define ,(cons (string->symbol (string-append "make-" namestr))
		    slots)
       (apply make-vector (cons 'vector-mixed slots)))
     ,@(map (lambda (slot)
	      `(begin (define ,(cons (string->symbol (string-append
						namestr
						(string-append "-"
							       (symbol->string (car slot)))))
			       (list 'struct))
		  (vector-ref struct (cdr slot)))
		(define ,(cons (string->symbol (string-append
						namestr
						(string-append "-"
							       (string-append
								(symbol->string (car slot))
								"-set!"))))
			       (list 'struct 'setter))
		  (vector-set! struct (cdr slot) setter))))
	    slotpairs))))
     
		  

	      
(define (vector=? a b)
  (define (vector=?-iter a b elem)
    (cond
     ((= (vector-len a) elem) true)
     (else
      (and (equal? (vector-ref a elem)
		   (vector-ref b elem))
	   (vector=?-iter a b (+ elem 1))))))
  (if (= (vector-len a) (vector-len b))
      (vector=?-iter a b 0)
      false))

(define string-concat vector-concat)
(define string-ref vector-ref)

(define (equal? a b)
  (cond
   ((eq? a b) true)
   ((and (pair? a) (pair? b))
    (and (equal? (car a) (car b))
	 (equal? (cdr a) (cdr b))))
   ((and (vector? a) (vector? b))
    (vector=? a b))
   (else
    false)))
