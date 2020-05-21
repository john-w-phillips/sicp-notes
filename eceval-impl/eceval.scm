(load "regsim.scm")
(load "environment.scm")
(load "eceval-ops.scm")
(load "eceval-syntax.scm")
(define (get-global-environment)
  the-global-environment)
(define (apply-primitive-procedure proc argl)
  (apply (primitive-procedure-proc proc) argl))

(define eceval-operations
  (list (list 'empty-arglist empty-arglist)
	(list 'read read)
	(list 'adjoin-arg adjoin-arg)
	(list 'last-operand? last-operand?)
	(list 'prompt-for-input prompt-for-input)
	(list 'announce-output announce-output)
	(list 'clear-output clear-output)
	(list 'user-print user-print)
	(list 'quoted? quoted?)
	(list 'extract-error-description extract-error-description)
	(list 'null? null?)
	(list 'variable? variable?)
	(list 'assignment? assignment?)
	(list 'definition? definition?)
	(list 'if? if?)
	(list 'symbol? symbol?)
	(list 'lambda? lambda?)
	(list 'begin? begin?)
	(list 'application? application?)
	(list 'lookup-variable-value lookup-variable-value)
	(list 'text-of-quotation text-of-quotation)
	(list 'lambda-parameters lambda-parameters)
	(list 'lambda-body lambda-body)
	(list 'make-procedure make-procedure)
	(list 'get-global-environment get-global-environment)
	(list 'set-variable-value! set-variable-value!)
	(list 'definition-variable definition-variable)
	(list 'definition-value definition-value)
	(list 'self-evaluating? self-evaluating?)
	(list 'operands operands)
	(list 'first-operand first-operand)
	(list 'rest-operands rest-operands)
	(list 'primitive-procedure? primitive-procedure?)
	(list 'compound-procedure? compound-procedure?)
	(list 'apply-primitive-procedure apply-primitive-procedure)
	(list 'operator operator)
	(list 'procedure-parameters procedure-parameters)
	(list 'procedure-environment procedure-environment)
	(list 'procedure-body procedure-body)
	(list 'extend-environment extend-environment)
	(list 'begin-actions begin-actions)
	(list 'first-exp first-exp)
	(list 'rest-exps rest-exps)
	(list 'last-exp? last-exp?)
	(list 'if-predicate if-predicate)
	(list 'if-consequent if-consequent)
	(list 'if-alternative if-alternative)
	(list 'true? true?)
	(list 'false? false?)
	(list 'assignment-variable assignment-variable)
	(list 'assignment-value assignment-value)
	(list 'no-operands? no-operands?)
	(list 'define-variable! define-variable!)))
	      

(define controller-text
  '(read-eval-print-loop
    (perform (op initialize-stack))
    (perform (op prompt-for-input) (const ";; EC-Eval input: "))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))
    print-result
    (perform (op print-statistics))
    (perform (op announce-output) (const ";; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))
    undefined-variable
    (assign val (const unknown-variable-name-error))
    (goto (label signal-error))
    unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))
    unknown-procedure-type
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))
    bad-number-of-arguments-error
    (assign val (const bad-number-of-arguments-error))
    (goto (label signal-error))
    no-if-predicate
    (assign val (const no-if-predicate))
    (assign unev (reg exp))
    (goto (label signal-error))
    malformed-if-alternative
    (assign val (const malformed-if-alternative))
    (assign unev (reg exp))
    (goto (label signal-error))
    malformed-if-consequent
    (assign val (const malformed-if-consequent))
    (assign unev (reg exp))
    (goto (label signal-error))
    malformed-set-statement
    (assign val (const malformed-set-statement))
    (goto (label signal-error))
    malformed-definition
    (assign val (const malformed-definition))
    (goto (label signal-error))
    malformed-syntax
    (assign val (const malformed-syntax-error))
    (assign unev (reg exp))
    (goto (label signal-error))
    primitive-error
    (assign val (const primitive-error))
    (goto (label signal-error))
    signal-error
    (perform (op user-print) (reg val))
    (perform (op clear-output))
    (assign val (op extract-error-description) (reg unev))
    (perform (op user-print) (reg val))
    (perform (op clear-output))
    (goto (label read-eval-print-loop))
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))
    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
    ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (test (op machine-error?) (reg val))
    (branch (label undefined-variable))
    (goto (reg continue))
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (test (op machine-error?) (reg unev))
    (branch (label malformed-syntax))
    (assign exp (op lambda-body) (reg exp))
    (test (op machine-error?) (reg unev))
    (branch (label malformed-syntax))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (goto (reg continue))
    ev-application
    (save continue)
    (assign unev (op operands) (reg exp))
    (assign exp (op operator) (reg exp))
    (test (op symbol?) (reg exp))
    (branch (label ev-lookup-symb))
    (save env) 
    (save unev)
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))
    ev-lookup-symb
    (assign proc (op lookup-variable-value) (reg exp) (reg env))
    (goto (label ev-appl-did-operator-no-restore))
    ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign proc (reg val))
    ev-appl-did-operator-no-restore
    (assign argl (op empty-arglist))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))
    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))
    primitive-apply
    (assign val (op apply-primitive-procedure)
	    (reg proc) (reg argl))
    (test (op machine-error?) (reg val))
    (assign unev (reg val))
    (branch (label primitive-error))
    (restore continue)
    (goto (reg continue))
    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
    (test (op machine-error?) (reg env))
    (branch (label bad-number-of-arguments-error))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))
    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))
    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    ;; (test (op null?) (reg unev))
    ;; (branch (label ev-sequence-last-exp))
    (assign exp (op first-exp) (reg unev))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))
    ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (test (op machine-error?) (reg exp))
    (branch (label no-if-predicate))
    (goto (label eval-dispatch))
    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (test (op machine-error?) (reg exp))
    (branch (label malformed-if-alternative))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (test (op machine-error?) (reg exp))
    (branch (label malformed-if-consequent))
    (goto (label eval-dispatch))
    ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (test (op machine-error?) (reg unev))
    (branch (label malformed-set-statement))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (test (op machine-error?) (reg exp))
    (branch (label malformed-syntax))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (assign val
	    (op set-variable-value!) (reg unev) (reg val) (reg env))
    (test (op machine-error?) (reg val))
    (branch (label undefined-variable))
    (assign val (const ok))
    (goto (reg continue))
    ev-definition
    (assign unev (op definition-variable) (reg exp))
    (test (op machine-error?) (reg unev))
    (branch (label malformed-definition))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (test (op machine-error?) (reg exp))
    (branch (label malformed-definition))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))
    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))))
  
(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   controller-text))

(define the-global-environment (setup-environment))
(start eceval)
