(define *op-table* (make-hash-table))

(define (put op type item)
  (cond
   ((hash-table-exists? *op-table* op)
    (has-table-set! (hash-table-ref *op-table* op) type item))
   (else
    (begin
      (let ((new-table (make-hash-table)))
	(hash-table-set! new-table type item)
	(hash-table-set! *op-table* op new-table))))))
(define (get op type)
  (hash-table-ref (hash-table-ref *op-table* op) type))
