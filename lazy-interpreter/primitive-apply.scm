(define (apply-primitive-procedure procedure arguments)
  (language-apply (cadr procedure) arguments))

