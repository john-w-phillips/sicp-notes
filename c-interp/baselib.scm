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
		      'slots)
	 (apply make-vector (cons 'vector-mixed (cons (quote ,name) slots))))
       ,@(map (lambda (slot)
		`(begin
		   (define ,(cons (string->symbol (string-append
						   namestr "?"))
				  (list 'struct))
		     (and (vector? struct)
			  (eq? (vector-ref struct 0) (quote ,name))))
		   (define ,(cons (string->symbol (string-append
						   namestr
						   (string-append "-"
								  (symbol->string (car slot)))))
				  (list 'struct))
		     (vector-ref struct ,(+ 1 (cdr slot))))
		   (define ,(cons (string->symbol (string-append
						   namestr
						   (string-append "-"
								  (string-append
								   (symbol->string (car slot))
								   "-set!"))))
				  (list 'struct 'setter))
		     (vector-set! struct ,(+ 1 (cdr slot)) setter))))
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
(define (> a b)
  (not (or (= a b) (< a b))))
(define (<= a b)
  (or (< a b) (= a b)))

(define (strings-concat . strs)
  (if (null? strs) ""
      (string-concat (car strs)
		     (apply strings-concat (cdr strs)))))
(define (memq elem alist)
  (if (null? alist) false
      (or (eq? elem (car alist))
	  (memq elem (cdr alist)))))

(define (reverse alist)
  (define (rev-iter inlist returnlist)
    (if (null? inlist) returnlist
	(rev-iter (cdr inlist) (cons (car inlist) returnlist))))
  (rev-iter alist '()))
(define cadr (lambda (x) (car (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cddr (lambda (x) (cdr (cdr x))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))

(define (vec-contains? avec anelem)
  (define (vec-iter i)
    (cond
     ((= i (vector-len avec)) false)
     ((equal? anelem (vector-ref avec i))
      true)
     (else
      (vec-iter (+ i 1)))))
  (vec-iter 0))

(define (substring a-string start-index length)
  (define (substring-iterator building-string current-index length-remaining)
    (if (= length-remaining 0) building-string
	(begin
	  (vector-push-back! building-string (vector-ref a-string current-index))
	  (substring-iterator
	   building-string
	   (+ 1 current-index)
	   (- length-remaining 1)))))
  (let ((new-string (make-vector 'char)))
    (substring-iterator new-string start-index length)))
(define string-len vector-len)
(define (string-substring-set! a-string
			       substring
			       starting-index
			       replacement-length)
  (define (substring-set-iterator current-index)
    (cond
     ((= current-index replacement-length) a-string)
     (else
      (vector-set! a-string
		   (+ starting-index current-index)
		   (vector-ref substring current-index))
      (substring-set-iterator
       (+ 1 current-index)))))
  (substring-set-iterator 0))

(define (string-replace a-string
			search-substring
			replacement-string)
  (let ((output-string (make-vector 'char)))
    (define (replacement-iterator current-index)
      (cond
       ((= current-index (string-len a-string))
	output-string)
       ((and
	 (not (> (string-len search-substring)
	    (- (string-len a-string) current-index)))
	 (string=? (substring a-string current-index (string-len search-substring))
		  search-substring))
	(begin
	  (vector-extend! output-string replacement-string)
	  (replacement-iterator (+ current-index (string-len search-substring)))))
       (else
	(vector-push-back! output-string (vector-ref a-string current-index))
	(replacement-iterator (+ current-index 1)))))
    (replacement-iterator 0)))
				
			
(define (string-copy input-string)
  (define (copy-iterator output-string index)
    (if (= index (string-len input-string))
	output-string
	(begin
	  (vector-push-back! output-string (vector-ref
					    input-string
					    index))
	  (copy-iterator output-string (+ index 1)))))
  (copy-iterator (make-vector 'char) 0))
	
