(define *op-table* (make-hash-table))

(define (put op type item)
  (cond
   ((hash-table-exists? *op-table* op)
    (hash-table-set! (hash-table-ref *op-table* op) type item))
   (else
    (begin
      (let ((new-table (make-hash-table)))
	(hash-table-set! new-table type item)
	(hash-table-set! *op-table* op new-table))))))
(define (get op type)
  (hash-table-ref (hash-table-ref *op-table* op) type))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

;; (define (attach-tag tag datum)
;;   (cons tag datum))

;; (define (type-tag datum)
;;   (car datum))

;; (define (contents datum)
;;   (cdr datum))

(define (attach-tag tag datum)
  (cond
   ((eq? 'scheme-number tag) datum)
   (else (cons tag datum))))

(define (type-tag datum)
  (cond
   ((number? datum) 'scheme-number)
   (else (car datum))))

(define (contents datum)
  (cond
   ((number? datum) datum)
   (else (cdr datum))))
