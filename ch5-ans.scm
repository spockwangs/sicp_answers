
;; 5.2

(controller
 test-counter
 (test (op >) (reg counter) (const 0))
 (branch (label done))
 (assign product (op *) (reg product) (reg counter))
 (assign counter (op +) (reg counter) (const 1))
 (goto (label test-counter))
 done)

;; 5.3

(controller
 test-guess
 (assign t (op square) (reg guess))
 (assign t (op -) (reg t) (reg x))
 (assign t (op abs) (reg t))
 (test (op <) (reg t) (const 0.001))
 (branch (label sqrt-done))
 (assign t (op /) (reg x) (reg guess))
 (assign guess (op average) (reg guess) (reg t))
 (goto (label test-guess))
 sqrt-done)

;; 5.4

(define expt-m1
  (make-machine '(continue n b val)
		(list (list '= =)
		      (list '* *)
		      (list '- -))
		'(controller
		  (assign continue (label expt-done))
		  expt
		  (test (op =) (reg n) (const 0))
		  (branch (label base-case))
		  (assign n (op -) (reg n) (const 1))
		  (save continue)
		  (assign continue (label after-expt))
		  (goto (label expt))
		  after-expt
		  (restore continue)
		  (assign val (op *) (reg b) (reg val))
		  (goto (reg continue))
		  base-case
		  (assign val (const 1))
		  (goto (reg continue))
		  expt-done)))

(define expt-m2
  (make-machine '(continue n b product val)
		(list (list '= =)
		      (list '- -)
		      (list '* *))
		'(controller
		  (assign continue (label expt-done))
		  (assign product (const 1))
		  expt-loop
		  (test (op =) (reg n) (const 0))
		  (branch (label base-case))
		  (assign n (op -) (reg n) (const 1))
		  (assign product (op *) (reg b) (reg product))
		  (goto (label expt-loop))
		  base-case
		  (assign val (reg product))
		  (goto (reg continue))
		  expt-done)))

;; 5.8

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (memq next-inst
			 (map car labels))
		   (error "Duplicate label -- " next-inst)
		   (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

;; 5.9

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                  (error "Using operation on label: " e)
                  (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; 5.11

b)

(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
	(push stack (cons reg-name (get-contents reg)))
	(advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let (reg (get-register machine reg-name))
      (lambda ()
	(let ((stack-top (pop stack)))
	  (let ((saved-reg-name (car stack-top))
		(saved-reg (cdr stack-top)))
	    (if (equal? reg-name saved-reg-name)
		(begin
		  (set-contents! reg saved-reg)
		  (advance-pc pc))
		(error "Restoring saved reg " saved-reg-name "into " reg-name))))))))

;; 5.16 

I’ll add a state variable into the large let at the top of make-new-machine:

(instruction-trace-on #f)

The new messages the machine accepts are:

((eq? message 'trace-on)
  (set! instruction-trace-on #t))
((eq? message 'trace-off)
  (set! instruction-trace-on #f))

And this is the new execute procedure (it augments both instruction counting and tracing):

(define (execute)
  (let ((insts (get-contents pc)))
    (if (null? insts)
      'done
      (begin
        (set! instruction-count (+ 1 instruction-count))
        (if instruction-trace-on
          (printf "trace: ~a~%" (instruction-text (car insts))))
        ((instruction-execution-proc (car insts)))
        (execute)))))

;; 5.38

I’ll add this new dispatch to compile:

((memq (car exp) '(+ - * / =))
  (compile-open-binary-op exp target linkage))

And the extra code is:

(define (spread-arguments a1 a2)
  (let ((ca1 (compile a1 'arg1 'next '()))
        (ca2 (compile a2 'arg2 'next '())))
    (list ca1 ca2)))

(define (compile-open-binary-op exp target linkage)
  (if (= (length exp) 3)
    (let ((op (car exp))
          (args (spread-arguments (cadr exp) (caddr exp))))
      (end-with-linkage linkage
        (append-instruction-sequences
          (car args)
          (preserving '(arg1)
            (cadr args)
            (make-instruction-sequence '(arg1 arg2) (list target)
              `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))
    (error "Expected a 3-element list -- COMPILE-OPEN-BINARY-OP" exp)))

(define (compile-open-arbitrary-op exp target linkage)
  (define (transform exp)
    (let ((op (car exp))
	  (args (cdr exp)))
      (define (loop args result)
	(cond ((null? args) result)
	      (else
	       (loop (cdr args)
		     (make-op-exp op result (car args))))))
      (define (make-op-exp op a1 a2)
	(list op a1 a2))
      (if (< (length args) 2)
	  (error "At least 2 arguments -- TRANSFROM" exp)
	  (loop (cddr args)
		(make-op-exp op (car args) (cadr args))))))
  (compile-open-binary-op (transform exp) target linkage))

;; 5.39

(define (lexical-address-lookup lexical-address run-env)
  (define (find-frame n env)
    (cond ((eq? env the-empty-environment) (error "frame not found"))
	  ((= n 0) (first-frame env))
	  (else (find-frame (- n 1) (enclosing-environment env)))))
  (define (find-val n values)
    (cond ((null? values) (error "value not found"))
	  ((= n 0)
	   (if (eq? (car values) '*unassigned)
	       (error "Variable is unassigned")
	       (car values)))
	  (else (find-val (- n 1) (cdr values)))))
  (find-val (cadr lexical-address)
	    (frame-values (find-frame (car lexical-address)
				      run-env))))

(define (lexical-address-set! lexical-address value run-env)
  (define (find-frame n env)
    (cond ((eq? env the-empty-environment) (error "frame not found -- find-frame"))
	  ((= n 0) (first-frame env))
	  (else (find-frame (- n 1) (enclosing-environment env)))))
  (define (scan-val n values)
    (cond ((null? values)
	   (error "Invalid lexical address -- lexical-address-set!" lexical-address))
	  ((= n 0)
	   (set-car! values value))
	  (else (scan-val (- n 1) (cdr values)))))
  (scan-val (cadr lexical-address)
	    (frame-values (find-frame (car lexical-address) run-env))))

;; 5.41

(define (find-variable var compile-env)
  (define (index var list n)
    (cond ((equal? var (car list)) (+ 1 n))
	  (else (index var (cdr list) (+ 1 n)))))
  (define (iter m env)
    (cond ((null? env) 'not-found)
	  ((memq var (car env))
	   (list m (index var (car env) -1)))
	  (else (iter (+ m 1)
		      (cdr env)))))
  (iter 0 compile-env))

;; 5.43

(define (compile-lambda-body exp proc-entry compile-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
          (assign env (op compiled-procedure-env) (reg proc))
          (assign env
                  (op extend-environment)
                  (const ,formals)
                  (reg argl)
                  (reg env))))
      (compile-sequence
        (scan-out-defines (lambda-body exp))  ;; change!
        'val 'return
        (extend-compile-env formals compile-env)))))

