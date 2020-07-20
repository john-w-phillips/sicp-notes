(load "baselib.scm")

(define-struct port (buffer fd))
(define BUFFER-MAXLEN 512)
(define (eof? obj) (eq? obj eof))
(define (flush-port p)
  (if (> (vector-len (port-buffer p)) 0)
      (let ((rval
	     (sys-write (port-fd p)
			(port-buffer p)
			(vector-len (port-buffer p)))))
	(vector-truncate! (port-buffer p) 0)
	rval)))

(define (open-port fname modestring)
  (let ((fd (sys-open fname modestring))
	(buffer (make-vector 'char)))
    (if (< fd 0)
	(error "Cannot open file!")
	(make-port buffer fd))))

(define (close-port p)
  (flush-port p)
  (sys-close (port-fd p)))

(define (write-to-port p buf)
  (cond
   ((> BUFFER-MAXLEN (+ (vector-len buf) (vector-len (port-buffer p))))
    (vector-extend! (port-buffer p) buf)
    (vector-len buf))
   ((< BUFFER-MAXLEN (vector-len buf))
    (let ((bigbuf (vector-concat (port-buffer p) buf)))
      (sys-write (port-fd p)
		 bigbuf
		 (vector-len bigbuf))
      (vector-truncate! (port-buffer p) 0)))
   (else
    (flush-port p)
    ;; set the contents of the internal buffer to the input buffer.
    (vector-truncate! (port-buffer p) 0)
    (vector-extend! (port-buffer p) buf))))

(define (read-from-port p n)
  (let ((rv (sys-read (port-fd p) n)))
    (if (= (car rv) 0)
	eof
	(cdr rv))))


