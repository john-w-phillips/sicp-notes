(load "syntax.scm")
(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))


(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
	 (let ((next-inst (car text)))
	   (if (symbol? next-inst)
	       (receive
		   insts
		   (cons (make-label-entry next-inst
					   insts)
			 labels))
	       (receive
		   (cons
		    (make-instruction next-inst)
		    insts)
		   labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-or-create-register machine 'pc))
	(flag (get-or-create-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations))
	(control-data (machine 'get-control-data)))
    (for-each
     (lambda (inst)
       ((control-data 'insert-item!)
	'instructions
	(instruction-text inst))
       (set-instruction-execution-proc!
	inst
	(make-execution-procedure
	 (instruction-text inst)
	 labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
	(cdr val)
	(error "Undefined label: ASSEMBLE"
	       label-name))))


(define (make-execution-procedure
	 inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto  inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else
	 (error "Unknown instruction type: ASSEMBLE"
		inst))))



(define (make-assign inst machine labels operations pc)
  (let ((control-data
	 (machine 'get-control-data))
	(target
	 (get-or-create-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp
		value-exp machine labels operations)
	       (make-primitive-exp
		(car value-exp) machine labels))))
      ((control-data 'insert-keyed-item!)
       'assignment-values
       (assign-reg-name inst)
       (if (and (pair? value-exp) (null? (cdr value-exp))) (car value-exp)
	   value-exp))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc
	       (make-operation-exp
		condition machine labels operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc pc)))
	(error "Bad TEST instruction: ASSEMBLE" inst))))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts
	       (lookup-label
		labels
		(label-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc pc))))
	(error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts (lookup-label
			 labels
			 (label-exp-label dest))))
	     (lambda ()
	       (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg (get-or-create-register
		       machine
		       (register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
	   

(define (make-save inst machine stack pc)
  (let ((reg (get-or-create-register machine
			   (stack-inst-reg-name inst))))
    (((machine 'get-control-data) 'insert-item!)
     'saved-regs
     (stack-inst-reg-name inst))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-or-create-register machine
			   (stack-inst-reg-name inst))))
    (((machine 'get-control-data) 'insert-item!)
     'restored-regs
     (stack-inst-reg-name inst))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))


(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc
	       (make-operation-exp
		action
		machine labels operations)))
	  (lambda () (action-proc) (advance-pc pc)))
	(error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c))) 
	((label-exp? exp)
	 (let ((insts (lookup-label
		       labels
		       (label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-or-create-register machine (register-exp-reg exp))))
	   (lambda () (get-contents r))))
	(else (error "Unknown expression type: ASSEMBLE" exp))))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
			 operations))
	(aprocs
	 (map (lambda (e)
		(if (label-exp? e)
		    (error "Cannot do machine operations on labels MAKE-OPERATION-EXP" e)
		    (make-primitive-exp e machine labels)))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
	(cadr val)
	(error "Unknown operation: ASSEMBLE"
	       symbol))))

