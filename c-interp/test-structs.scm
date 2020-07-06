(load "baselib.scm")
(define-struct hello-struct (a b c d))
(define s (make-hello-struct 'a 'b 1 2))
(assert (eq? (hello-struct-a s) 'a))
(assert (eq? (hello-struct-b s) 'b))
(assert (= (hello-struct-c s) 1))
(assert (= (hello-struct-d s) 2))
(hello-struct-b-set! s 'c)
(assert (hello-struct? s))

(assert (eq? (hello-struct-b s) 'c))
(define-struct struct2 (l m n o))
(define s2 (make-struct2 0 0 'a 'a))
(assert (struct2? s2))
