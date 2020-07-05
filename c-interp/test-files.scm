(load "baselib.scm")

(define (file-test filename)
  (let ((fd (sys-open filename "w")))
    (assert (= 3 (sys-write fd "abc" 3)))
    (assert (= 4 (sys-write fd "defg" 4)))
    (sys-close fd))
  (let ((fd (sys-open filename "r")))
    (assert (string=? "abc" (cdr (sys-read fd 3))))
    (assert (string=? "defg" (cdr (sys-read fd 4))))))

(file-test "hello.txt")
