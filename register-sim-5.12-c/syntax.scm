(define (tagged-list? exp name)
  (and (pair? exp)
       (eq? (car exp) name)))
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (perform-action inst)
  (cdr inst))


(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))
