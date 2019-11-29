(define (tagged-list? exp name)
  (and (pair? exp)
       (eq? (car exp) name)))

(define (assign? inst)
  (pair? inst))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cons
   (car assign-instruction)
   (cddr assign-instruction)))

(define (test? inst)
  (tagged-list? inst 'test))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))
(define (branch? inst)
  (tagged-list? inst 'branch))


(define (goto-dest goto-instruction)
  (cadr goto-instruction))
(define (goto? inst)
  (tagged-list? inst 'goto))


(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))
(define (perform? inst)
  (tagged-list? inst 'perform))
(define (perform-action inst)
  (cdr inst))


(define (register-exp? exp machine)
  ((machine 'has-register?) exp))

(define (register-exp-reg exp) exp)

(define (constant-exp? exp)
  (or (number? exp) (string? exp)))

(define (constant-exp-value exp) exp)

(define (label-exp? exp labels)
  (and (symbol? exp)
       (has-label? labels exp)))

(define (label-exp-label exp) exp)

(define (operation-exp? exp ops)
  (and (pair? exp)
       (not (false? (assoc (car exp) ops)))))

(define (operation-exp-op operation-exp)
  (car operation-exp))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (save? exp)
  (tagged-list? exp 'save))
(define (restore? exp)
  (tagged-list? exp 'restore))
