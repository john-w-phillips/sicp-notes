(define (require x) (if (not x) (amb)))
(define (filter proc items)
  (cond
   ((null? items) '())
   ((proc (car items))
    (cons (car items) (filter proc (cdr items))))
   (else (filter proc (cdr items)))))

(define (make-relation father daughter boat-name)
  (list father daughter boat-name))
(define (relation-father r)
  (car r))
(define (relation-daughter r)
  (cadr r))
(define (relation-boat-name r)
  (caddr r))

(define (require-sane-single relation)
  (require (not (eq? (relation-daughter relation)
		     (relation-boat-name relation)))))
(define (require-no-multi-fathers relation relations)
  (let ((father (relation-father relation)))
    (require (null? (filter 
		     (lambda (r) (eq? (relation-father r) father))
		     relations)))))
 
(define (require-no-multi-daughters relation relations)
  (let ((daughter (relation-daughter relation)))
    (require (null? (filter 
		     (lambda (r) (eq? (relation-daughter r) daughter))
		     relations)))))

(define (require-sane-relations relation-list)

  (cond
   ((null? relation-list) true)
   (else
    (require-sane-single (car relation-list))
    (require-no-multi-fathers (car relation-list) (cdr relation-list))
    (require-no-multi-daughters (car relation-list) (cdr relation-list))
    (require-sane-relations (cdr relation-list)))))

(let ((base-relations
       (list (make-relation 'mr-moore 'mary-ann 'lorna)
	     (make-relation 'cd 
			    (amb 'mary-ann 'lorna 'gabrielle 'rosalind 'melissa)
			    'melissa)
	     (make-relation 'mr-hall
			    (amb 'mary-ann 'lorna 'gabrielle 'rosalind 'melissa)
			    'rosalind)
	     (make-relation 'dr-parker
			    (amb 'mary-ann 'lorna 'gabrielle 'rosalind 'melissa)
			    (amb 'mary-ann 'lorna 'gabrielle 'rosalind 'melissa))
	     (make-relation 'sir-barnacle
			    'melissa
			    'gabrielle))))
  (require-sane-relations base-relations)
  (let ((lornas-father-relations
	 (filter
	  (lambda (r)
	    (eq? (relation-daughter r) 'lorna))
	  base-relations)))
    (relation-father (car lornas-father-relations))))
 
