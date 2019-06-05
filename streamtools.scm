(define (stream->list s n)
  (define (iter i s)
    (cond
     ((or (> i n) (empty-stream? s))
      '())
     (else
      (cons (stream-car s)
	    (iter (+ i 1) (stream-cdr s))))))
  (iter 0 s))

(define (scale-stream s n)
  (stream-map (lambda (x) (* n x)) s))

(define (add-streams . streams)
  (cons-stream
   (apply + (map stream-car streams))
   (apply add-streams
    (map stream-cdr streams))))
