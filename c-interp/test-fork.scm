(load "baselib.scm")
(let ((pid (sys-fork)))
  (if (= pid 0)
      (sys-exec "/bin/echo" "hello")))

(let ((pid (sys-fork)))
  (if (= pid 0)
      (sys-exec "/bin/echo" "hello2")))

