;; vi:expandtab:shiftwidth=4:tabstop=4: 
;; 4.1

; evaluate from left to right

(define (list-of-values-4.1 exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval-wbb (first-operand exps) env)))
	(let ((rest-value (list-of-values-4.1 (rest-operands exps) env)))
	  (cons first-value rest-value)))))

; evaluate operands from right to left

(define (list-of-values-4.1 exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-value (list-of-values-4.1 (rest-operands exps) env)))
	(let ((first-value (eval-wbb (first-operand exps) env)))
	  (cons first-value rest-value)))))

;; 4.2

(define (application?-4.2 exp)
  (tagged-list? exp 'call))

(define (operator-4.2 exp)
  (cadr exp))

(define (operands-4.2 exp)
  (cddr exp))

;; 4.3

(define (install-eval-packages)
  (define (eval-quoted exp env)
    (text-of-quotation exp))
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp env)))
  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (put 'eval 'quote eval-quoted)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond)
  'done)

(define (eval-wbb exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get 'eval (car exp))
	 ((get 'eval (car exp)) exp env))
	((application? exp)
	 (apply-wbb (eval-wbb (operator exp) env)
		 (list-of-values (operands exp) env)))
	(else
	 (error "Unkown expression type -- EVAL" exp))))


;; 4.4

(define (and? exp) (tagged-list? exp 'and))

(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (eval-and-clauses clauses env)
    (if (null? clauses)
	true
	(if (eval (car clauses) env)
	    (eval-and-clauses (cdr clauses) env)
	    false)))
  (eval-and-clauses (cdr exp) env))

(define (eval-or exp env)
  (define (eval-or-clauses clauses env)
    (if (null? or-clauses)
	false
	(if (eval (car or-clauses) env)
	    true
	    (eval-or-clauses (cdr clauses) env))))
  (eval-or-clauses (cdr exp) env))

(define (and->if exp)
  ;; 将and表达式转换为if表达式
  (define (expand-clauses clauses)
    (if (null? (cdr clauses))
	(make-if clauses 'true 'false)
	(make-if (car clauses)
		 (expand-clauses (cdr clauses))
		 'false)))
  (expand-clauses (cdr exp)))

(define (or->if exp)
  ;; 将or表达式转换为if表达式
  (define (expand-clauses clauses)
    (if (null? (cdr clauses))
	(make-if clauses 'true 'false)
	(make-if (car clauses)
		 'true
		 (expand-clauses (cdr clauses)))))
  (expand-clauses (cdr exp)))

;; 4.5

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)         ; no else clause
	'false
	(let ((first (car clauses))
	      (rest (cdr clauses)))
	  (if (cond-else-clause? first)     ; else clause
	      (if (null? rest)
		  (sequence->exp (cond-actions first))
		  (error "ELSE clause isn't last -- COND->IF" clauses))
	      (if (and (= (length first) 3)
		       (eq? (cadr first) '=>))    ; extended cond syntax
		  (make-if (car first)
			   (list (caddr first) (car first))
			   (expand-clauses rest))
		  (make-if (cond-predicate first)
			   (sequence->exp (cond-actions first))
			   (expand-clauses rest)))))))
  (expand-clauses (cond-clauses exp)))

;; 4.6

(define (let? exp) (tagged-list? 'let exp))

(define (let-variables exp)
  (map car (cadr exp)))

(define (let-exps exp)
  (map cadr (cadr exp)))

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp)
		     (let-body exp))
	(let-exps exp)))

;; 4.7

(define (let*-body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  ;; 将let*表达式转换成嵌套的let表达式
  (define (expand-clauses clauses)
    (if (null? (cdr clauses))
	(make-let (list (car clauses))
		  (let*-body exp))
	(make-let (list (car clauses))
		  (list (expand-clauses (cdr clauses))))))
  (expand-clauses (cadr exp)))

在eval.中加入以下语句即可

((let*? exp)
 (eval. (let*->nested-lets exp) env))

;; 4.8

(define (named-let-def exp)
  (cadr exp))

(define (named-let-formal-para exp)
  (map car (caddr exp)))

(define (named-let-parameters exp)
  (map cadr (caddr exp)))

(define (named-let-body exp)
  (cdddr exp))

(define (let->combination exp)
  (if (pair? (cadr exp))        ; standard `let'
      (cons (make-lambda (let-variables exp)
			 (let-body exp))
	    (let-exps exp))
      ; named let
      (sequence->exp
       (list
	(list 'define
	      (named-let-def exp)
	      (make-lambda (named-let-formal-para exp)
			   (named-let-body exp)))
	(cons (named-let-def exp)
	      (named-let-parameters exp))))))

;; 4.9

; while expression has the following form
; (while <while-condition> <while-body>)

(define (while? exp) (tagged-list? exp 'while))

(define (while-condition exp) (cadr exp))

(define (while-body exp) (caddr exp))

(define (while->combination exp)
  (sequence->exp
   (list
    (list
     'define
     (list 'while-iter)
     (make-if (while-condition exp)
	      (sequence->exp (list (while-body exp)
				   (list 'while-iter)))
	      'true))
    (list 'while-iter))))

; add following to eval.:
((while? exp)
 (eval. (while->combination exp) env))

;; 4.10
Excercise 4.2 b) is such an example.

;; 4.11

(define (make-frame variables values)
  (define (iter vars vals)
    (cond ((null? vars) '())
	  ((symbol? vars)
	   (list (cons vars (cdr vals))))
	  (else
	   (cons (cons (car vars) (car vals))
		 (iter (cdr vars) (cdr vals))))))
  (cond ((list? variables)
	 (if (not (= (length variables)
		     (length values)))
	     (error "length mismatch -- MAKE-FRAME" variables values)
	     (iter variables values)))
	(else (iter variables values))))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (extend-environment vars vals env)
  (cons (make-frame vars vals) env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
	     (env-loop (enclosing-environment env)))
	    ((eq? (car (car frame)) var)
	     (cdr (car frame)))
	    (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- " var)
	(scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car (car frame)))
	     (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- " var)
	(scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((top-frame (car env)))
    (define (scan frame)
      (cond ((null? frame)
	     (add-binding-to-frame! var val top-frame))
	    ((eq? var (caar frame))
	     (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
    (scan top-frame)))

;; 4.12

(define (find-binding-in-frame frame var)
  ;; look up the var in the frame.
  ;; If found, return the corresponding value;
  ;; otherwise, return false
  (define (scan vars vals)
    (cond ((null? vars) false)
	  ((and (pair? vars)
		(eq? var (car vars)))
	   (car vals))
	  ((symbol? vars)
	   (if (eq? var vars)
	       vals
	       false))
	  (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
	(frame-values frame)))

(define (set-binding-in-frame! frame var val)
  ;; Sets the var to the val in the frame.
  ;; Returns true if the var is found and modified,
  ;; false otherwise.
  (define (scan vars vals)
    (cond ((null? vars) false)
	  ((and (pair? vars)
		(eq? var (car vars)))
	   (set-car! vals val)
	   true)
	  ((symbol? vars)
	   (if (eq? var vars)
	       (begin (set! vars val) true)
	       false))
	  (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
	(frame-values frame)))

(define (add-binding-to-frame! frame var val)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
; 以上为与具体frame结构有关的过程定义。以下过程在上述过程的基础上
; 定义，与frame的具体实现结构无关。

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((result (find-binding-in-frame
		       (first-frame env)
		       var)))
	  (or result
	      (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(or (set-binding-in-frame! (first-frame env))
	    (env-loop (enclosing-environment env)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (or (set-binding-in-frame! frame var val)
	(add-binding-to-frame! frame var val))))

;; 4.13

(define (unbind-var-in-frame! frame var)
  ;; Ubinds the var in the frame.
  (define (scan vars vals)
    (cond ((null? (cdr vars))
	   (error "Unbound variable -- UNBIND" var))
	  ((eq? (cadr vars) var)
	   (set-cdr! vars (cddr vars))
	   (set-cdr! vals (cddr vals))
	   true)
	  (else (scan (cdr vars) (cdr vals)))))
  (let ((p-vars (frame-variables frame))
	(p-vals (frame-values frame)))
    (cond ((null? p-vars)
	   (error "Unbound variable -- UNBIND" var))
	  ((eq? (car p-vars) var)
	   (set-car! frame (cdr p-vars))
	   (set-cdr! frame (cdr p-vals))
	   true)
	  (else (scan p-vars p-vals)))))

(define (make-unbound? exp)
  ;; make-unbound! has the form (make-unbound! <var>)
  (tagged-list? exp 'make-unbound!))

(define (unbind-variable! exp env)
  (unbind-var-in-frame! (first-frame env) (cadr exp)))

; add the following to eval.:

((make-unbound? exp)
 (unbind-variable! exp env))

;; 4.15

Turing's halting theorem.

;; 4.16

;a)

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((pair? vars)
	     (if (eq? var (car vars))
		 (if (eq? (car vals) '*unassigned*)  ; check if the value is equal to *unassigned*?
		     (error "Unassigned variable -- " var)
		     (car vals))
		 (scan (cdr vars) (cdr vals))))
	    ((eq? var vars)
	     vals)
	    (else (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

; b)

(define (scan-out-defines procedure-body)
  (let ((vars '())
	(vals '())
	(rest '()))
    (for-each (lambda (exp)
		(if (definition? exp)
		    (begin (set! vars (append vars (list (definition-variable exp))))
			   (set! vals (append vals (list (definition-value exp)))))
		    (set! rest (append rest (list exp)))))
	      procedure-body)
    (if (null? vars)
	rest
	(list (make-let
	       (map (lambda (var)
		      (list var ''*unassigned*))
		    vars)
	       (append (map (lambda (var val)
			      (make-assignment var val))
			    vars
			    vals)
		       rest))))))

; c)

It is better install `scan-out-defines' in make-procedural.

When we define an new procedure, we'll call make-procedural. If
we install `scan-out-defines' in make-procedural, then the transformed
procedural definition will be saved in the global environment. So when we
apply this procedure later each time, we don't need to call `scan-out-defines'
to transform the procedure body.

But if we install `scan-out-defines' in `procedure-body', each time when we
apply an procedure, we have to call `scan-out-defines'. Obviously it is inefficient.

;; 4.17

(define (scan-out-defines procedure-body)
  (let ((vars '())
	(vals '())
	(rest '()))
    (for-each (lambda (exp)
		(if (definition? exp)
		    (begin (set! vars (append vars (list (definition-variable exp))))
			   (set! vals (append vals (list (definition-value exp)))))
		    (set! rest (append rest (list exp)))))
	      procedure-body)
    (if (null? vars)
	rest
	(append (map (lambda (var)
		       (make-define var ''*unassigned*))
		     vars)
		(append (map (lambda (var val)
			       (make-assignment var val))
			     vars
			     vals)
			rest)))))

;; 4.18

It won't work. When we evaluate the second definition, y is'nt
defined (which is assigned `*unassigned*').
If we use the text's way, it will work. The success depends on
the order of the definition.

;; 4.20

; a)

(define (letrec-clause exp)
  (cadr exp))

(define (letrec-body exp)
  (cddr exp))

(define (letrec->let exp)
  (let ((initials (letrec-clause exp))
	(body (letrec-body exp)))
    (make-let (map (lambda (initial)
		     (list (car initial) ''*unassigned*))
		   initials)
	      (append (map (lambda (initial)
			     (make-assignment (car initial) (cadr initial)))
			   initials)
		      body))))

;; 4.21

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
	  1
	  (* k (ft ft (- k 1)))))))
 10)

; a)

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (ft k)
      (cond ((or (= k 1)
		 (= k 2))
	     1)
	    (else
	     (+ (ft ft (- k 1))
		(ft ft (- k 2))))))))
 10)

; b)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
	 true
	 (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
	 false
	 (ev? ev? od? (- n 1))))))

;; 4.22

; add the following to analyze
((let? exp)
 (analyze (let->combination exp)))

;; 4.24

; original version
(interpret '(define (factorial n)
	      (if (= n 1)
		  1
		  (* (factorial (- n 1)) n))))

(time (repeat (interpret '(factorial 50)) 1000))
run time: 40.04
gc time: 1.18
real time: 105.225

; analyzed version
(interpret-ana '(define (factorial n)
		  (if (= n 1)
		      1
		      (* (factorial (- n 1)) n))))

(time (repeat (interpret-ana '(factorial 50)) 1000))
run time: 25.13
gc time: .77
real time: 64.705

;; 4.25

With applicative order it'll involved infinite loop.
With normal order it'll work.

;; 4.26

; add following to analyze
((unless? exp) (analyze (unless->if exp)))

(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless->if exp)
  (make-if (cadr exp)
	   (cadddr exp)
	   (caddr exp)))

When unless is special form, it can't work with high-order
procedure. For example, (apply unless ... )

;; 4.27

First response is: 1.
Second response is: 10.
Third response is: 2.

When defining w, (id (id 10)) is evaluated. So count is incremented and
becomes 1. When printing w, w is forced and (id 10) is evaluated. So
count is incremented again and becomes 2.

;; 4.28

If we don't get the actual value of the operator, the apply procedure
won't identify it as a procedure (because it's a thunk if we delay it) 
and can't apply it.

;; 4.29

For no memory version, the first response is 100 and the second is
2. For memory version the first is the same but the second is 1.
Because in the body of procedure square x is evaluated twice.
For memory version x is evaluated only once. When we evaluate x the
second time, we just get it from the memory. So count is incremented
only once.

;; 4.30

;a)
eval is called on each expression of the sequence. Since for-each uses 
begin which is evaluated as a sequence, each iteration of the for-each 
is evaluated with eval and since display is a primitive procedure, the 
actual value of its argument is eventually computed.

;b)
With original eval-sequence:
(p1 1)
==> (1 2)
(p2 1)
==> 1

When p in p2 is called, its argument `(set! x (cons x '(2)))' is not
actually evlauted but a thunk which binds with e. So e is a thunk.
The body of p is evaluated in sequence. When e is evaluated, its bound
thunk is substituted instead of it, but its value isn't passed to any
primitive procedure, it is not forced and the assignment doesn't really
happen. So later, when x evaluated and returned, it has its old value.

With Cy's proposed eval-sequence:
(p1 1)
==> (1 2)
(p2 1)
==> (1 2)

This is because e is forced in the sequence even without being passed
to a primitive procedure.

;d)
I prefer Cy' approach because it makes procedure more understandable.

;; 4.31

(define (lazy-operand-name operand)
  (car operand))
  
(define (evaluated-operand? operand)
  (symbol? operand))

(define (lazy-operand? operand)
  (and (pair? operand)
       (or (eq? (cadr operand) 'lazy)
	   (eq? (cadr operand) 'lazy-memo))))

(define (lazy-memo-operand? operand)
  (and (pair? operand)
       (eq? (cadr operand) 'lazy-memo)))

(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (force-it obj)
  (cond ((thunk-memo? obj)
	 (let ((result (actual-value
			(thunk-exp obj)
			(thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result)
	   (set-cdr! (cdr obj) '())
	   result))
	((thunk? obj)
	 (actual-value (thunk-exp obj) (thunk-env obj)))
	((evaluated-thunk? obj)
	 (thunk-value obj))
	(else obj)))

(define (lazy-apply proc args env)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure
	  proc
	  (list-of-arg-values args env)))
	((compound-procedure? proc)
	 (eval-sequence
	  (procedure-body proc)
	  (setup-arguments-env
	   (procedure-parameters proc)
	   args
	   env
	   (procedure-environment proc))))
	(else (error "Unkown procedure type -- APPLY" proc))))

(define (setup-arguments-env parameters args arg-env proc-env)
  (define (loop ops values vars vals)
    (cond ((null? ops)
	   (extend-environment vars vals proc-env))
	  (else
	   (let ((op (car ops))
		 (val (car values)))
	     (cond ((lazy-memo-operand? op)
		    (loop (cdr ops)
			  (cdr values)
			  (append vars (list (lazy-operand-name op)))
			  (append vals (list (delay-it-memo val arg-env)))))
		   ((lazy-operand? op)
		    (loop (cdr ops)
			  (cdr values)
			  (append vars (list (lazy-operand-name op)))
			  (append vals (list (delay-it val arg-env)))))
		   (else
		    (loop (cdr ops)
			  (cdr values)
			  (append vars (list op))
			  (append vals (list (actual-value val arg-env))))))))))
  (loop parameters args '() '()))

;; 4.33

(define (text-of-quotation exp env)
  (define (make-lazy-list items)
    (if (null? items)
	'()
	(list 'cons
	      (car items)
	      (make-lazy-list (cdr items)))))
  (let ((quote-operand (cadr exp)))
    (if (pair? quote-operand)
	(lazy-eval (make-lazy-list quote-operand) env)
	quote-operand)))

;; 4.35

(define (an-integer-between low high)
  (require (not (> low high)))
  (amb low (an-integer-between (+ low 1) high)))

;; 4.36

(define (pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; 4.37

Ben's method is more efficient because he shrinks the searching range.

;; 4.38

(define (multiple-dwelling-4.38)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require
     (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))
         
;; 4.39

(1) 约束条件的顺序不会影响答案，因为答案会满足所有约束条件。
(2) 约束条件的顺序会影响找到答案的速度。如下：
(define (md2)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (require (not (= baker 5)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require
     (distinct? (list baker cooper fletcher miller smith))) ; 
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

Runs more than twice as fast as the one given in the book. Note
that the requirement of distinctness was moved to the end. distinct?
is a function that runs in quadratic time, and so moving it to the
end saves us time because it is called less. When located first, it
is continuously called until satisfied, on many possible 5-tuples.
In the end, many possibilities were thrown out by the earlier
restrictions and distinct? takes less time to run.

;; 4.40

(define (md3)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require
            (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
		(list 'cooper cooper)
		(list 'fletcher fletcher)
		(list 'miller miller)
		(list 'smith smith)))))))

;; 4.41

(define (multiple-dwelling-4.41)
  (define (iter baker cooper fletcher miller smith)
    (if (and (distinct? (list baker cooper fletcher miller smith))
	     (not (= baker 5))
	     (not (= cooper 1))
	     (not (= fletcher 5))
	     (not (= fletcher 1))
	     (> miller cooper)
	     (not (= (abs (- smith fletcher)) 1))
	     (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith))
	(cond ((< smith 5)
	       (iter baker cooper fletcher miller (+ smith 1)))
	      ((< miller 5)
	       (iter baker cooper fletcher (+ miller 1) 1))
	      ((< fletcher 5)
	       (iter baker cooper (+ fletcher 1) 1 1))
	      ((< cooper 5)
	       (iter baker (+ cooper 1) 1 1 1))
	      ((< baker 5)
	       (iter (+ baker 1) 1 1 1 1))
	      (display "no more answer"))))
  (iter 1 1 1 1 1))

(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

;; 4.42

(define (exam-order-4.42)
  (let ((betty (amb 1 2 3 4 5))
	(ethel (amb 1 2 3 4 5))
	(joan (amb 1 2 3 4 5))
	(kitty (amb 1 2 3 4 5))
	(mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (require (or (and (= kitty 2) (not (= betty 3)))
		 (and (not (= kitty 2)) (= betty 3))))
    (require (or (and (= ethel 1) (not (= joan 2)))
		 (and (not (= ethel 1)) (= joan 2))))
    (require (or (and (= joan 3) (not (= ethel 5)))
		 (and (not (= joan 3)) (= ethel 5))))
    (require (or (and (= kitty 2) (not (= mary 4)))
		 (and (not (= kitty 2)) (= mary 4))))
    (require (or (and (= mary 4) (not (= betty 1)))
		 (and (not (= mary 4)) (= betty 1))))
    (list (list 'betty betty)
	  (list 'ethel ethel)
	  (list 'joan joan)
	  (list 'kitty kitty)
	  (list 'mary mary))))

;answer: ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

;; 4.43

(define (t-4.33 yacht daughter)
  (cons yacht daughter))

(define (yacht t)
  (car t))

(define (daughter t)
  (cdr t))

(define (father-puzzle)
  (let ((moore (t-4.33 'lorna 'mary))
	(barnacle (t-4.33 'gabrielle 'melissa))
	(downing (t-4.33 'melissa (amb 'lorna 'rosalind 'gabrielle)))
	(hall (t-4.33 'rosalind (amb 'gabrielle 'lorna))))
    (require (distinct? (list (daughter downing) (daughter hall))))
    (let ((parker (t-4.33 'mary (amb 'lorna 'rosalind)))
	  (gabrielle-father (amb downing hall)))
      (require (equal? (daughter gabrielle-father) 'gabrielle))
      (require (equal? (yacht gabrielle-father) (daughter parker)))
      (require (distinct? (list (daughter downing)
				(daughter hall)
				(daughter parker))))
      (list (list 'moore (yacht moore) (daughter moore))
	    (list 'barnacle (yacht barnacle) (daughter barnacle))
	    (list 'parker (yacht parker) (daughter parker))
	    (list 'downing (yacht downing) (daughter downing))
	    (list 'hall (yacht hall) (daughter hall))))))

;;answer: ((moore lorna mary) (barnacle gabrielle melissa) (parker mary rosalind) (downing melissa lorna) (hall rosalind gabrielle))

So Colonel Downing is the father of Lorna.

;; 4.44

(define (queens-4.44 board-size)
  (define (safe? q position)
    (if (null? position)
        true
        (let ((old-queen (car position)))
          (and (not (= old-queen q))
               (not (= (abs (- old-queen q)) (length position)))
               (safe? q (cdr position))))))
  (define (adjoin-new-queen new-queen position)
    (append position (list new-queen)))
  (define (queen-cols q-ref)
    (if (= q-ref 0)
        '()
        (let ((old-position (queen-cols (- q-ref 1))))
          (let ((n-q (an-integer-between 1 board-size)))
            (require (safe? n-q old-position))
            (adjoin-new-queen n-q old-position)))))
  (queen-cols board-size))


;; 4.47

Louis的建议是正确的。当改变amb表达式里的参数顺序:
(define (parse-verb-phrase)
  (amb
   (list 'verb-phrase
	 (parse-verb-phrase)
	 (parse-prepositional-phrase)))
  (parse-word verbs))
这个程序会进入无穷循环。当调用amb时，它会选择第一个选项，而第一个选项
会再次调用amb，因而会进入无穷循环。

;; 4.51

(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))

(define (permanent-set-var exp)
  (cadr exp))

(define (permanent-set-value exp)
  (caddr exp))

(define (analyze-permanent-set exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-variable exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (set-variable-value! var val env)
	       (succeed 'ok
			(lambda ()
			  (fail2))))
	     fail))))

;; 4.52

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-consequence exp)
  (cadr exp))

(define (if-fail-alternative exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (let ((cproc (analyze (if-fail-consequence exp)))
	(aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (cproc env
	     succeed
	     (lambda ()
	       (aproc env succeed fail))))))

;; 4.53

((8 35) (3 110) (3 20))

;; 4.54

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (not pred-value)
		   (fail2)
		   (succeed 'ok fail2)))
	     fail))))

;; 4.55

; a)
(supervisor ?x (Ben Bitdiddle))

; b)
(job ?x (accounting . ?y))

; c)
(address ?x (Slumerville . ?y))

;; 4.56

; a)
(and (supervisor ?x (Ben Bitdiddle))
     (address ?x ?y))

; b)
(and (salary ?person ?amount)
     (salary (Ben Bitdiddle) ?amount2)
     (lisp-value < ?amount ?amount2))

; c)
(and (supervisor ?person ?boss)
     (not (job ?person (computer . ?x)))
     (job ?boss ?bossjob))
	  

;; 4.57

(rule (substitute ?person1 ?person2)
      ;; person1 can substitute person2
      (and (or (and (job ?person1 ?job)
		    (job ?person2 ?job))
	       (and (job ?person1 ?job1)
		    (job ?person2 ?job2)
		    (can-do-job ?job1 ?job2)))
	   (not (same ?person1 ?person2))))

; a)
(substitute ?person (Fect Cy D))

; b)
(and (substitute ?self ?person)
     (salary ?self ?amount1)
     (salary ?person ?amount2)
     (lisp-value > ?amount2 ?amount1))

;; 4.58

(rule (big-shot ?big ?division)
      (and (job ?big (?division . ?x))
	   (or (not (supervisor ?big ?boss))
	       (and (supervisor ?big ?boss)
		    (not (job ?boss (?division . ?q)))))))

;; 4.59

; a)
(meeting ?division (Friday ?time))

; b)
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
	  (and (job ?person (?division . ?x))
	       (meeting ?division ?day-and-time))))

; c)
(and (meeting-time (Hacker Alyssa P) (Wednesday ?time))
     (meeting ?division (Wednesday ?time)))

;; 4.60

(rule (lives-near ?person1 ?person2)
      (and (address ?person1 (?town . ?rest-1))
	   (address ?person2 (?town . ?rest-2))
	   (lisp-value 'string<?
		       (write-to-string ?person1)
		       (write-to-string ?person2))))
;; 4.61

(assert! (rule (?x next-to ?y in (?x ?y . ?u))))

(assert! (rule (?x next-to ?y in (?v . ?z))
	       (?x next-to ?y in ?z)))

(?x next-to ?y in (1 (2 3) 4))
=>
((2 3) next-to 4 in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))
=>
(3 next-to 1 in (2 1 3 1))
(2 next-to 1 in (2 1 3 1))

;; 4.62

(assert! (rule (last-pair (?x) (?x))))

(assert! (rule (last-pair (?x . ?r) ?l)
	       (last-pair ?r ?l)))

;; 4.63

(rule (grandson ?grandpa ?grandson)
      (and (son ?grandpa ?son)
	   (son ?son ?grandson)))

(rule (son ?parent ?son)
      (or (and (wife ?parent ?h)
	       (son ?h ?son))
	  (son ?parent ?son)))

;; 4.64

outranked-by will call outranked-by because it is the first query statement of the 
and query. So it invokes a infinite loop.

;; 4.65

Oliver Warbucks is listed four times because the rule matches four times over the
 database. i.e., there are four middle-managers of whom Oliver Warbucks is a manager. 

;; 4.66

Obviously, if Ben tries to apply the sum on ?amount over these results, he’ll get
 too much because of the duplications.

One way to circumvent this problem is to install a uniqueness filter that will filter 
out all the duplicates from the resulting frames produced by qeval.

;; 4.68

(rule (reverse () ()))

The following version can evaluate (reverse ?x (1 2 3)) but not (reverse (1 2 3) ?x).
(rule (reverse (?x . ?y) ?r)
      (and (append-to-form ?z (?x) ?r)
	   (reverse ?y ?z)))

The following version can evaluate (reverse (1 2 3) ?x) but no (reverse ?x (1 2 3)).
(rule (reverse (?x . ?y) ?r)
      (and (reverse ?y ?z)
	   (append-to-form ?z (?x) ?r)))

;; 4.69

(rule (ends-with-grandson ?x)
      (append-to-form ?head (grandson) ?x))

(rule ((great . ?rel) ?x ?y)
      (and
        (ends-with-grandson ?rel)
        (?rel ?sx ?y)
        (son ?x ?sx)))

;; 4.70

THE-ASSERTIONS will become a infinite stream.

;; 4.71

These delays can postpone infinite looping in some cases, providing more meaningful 
answers to the user. Consider these assertions:

(qinterpret
  '(assert! (dummy a))
  '(assert! (dummy b))
  '(assert! (dummy c))
  '(assert! (rule (dummy ?x) (dummy ?x))))

The last one asserts an infinitely-recursive rule. Now, if we issue the query:

(qinterpret
  '(dummy ?who))

With the delays in place, we’ll get:

(DUMMY C)
(DUMMY B)
(DUMMY A)
(DUMMY C)
(DUMMY B)
(DUMMY A)
(DUMMY C)
(DUMMY B)
(DUMMY A)
...
...

To understand why, look at this portion of simple-query:

      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))

Note that the application of rules is delayed. In our case, there are valid assertions 
answering the query, but the rule is invalid, since trying to apply it causes an infinite loop.

Without the delay we get:

*** - Program stack overflow. RESET

Without any useful output, because simple-query deepens more and more into the recursive 
rule without ever printing results.

;; 4.72

As the hint suggests, this is done for exactly the same reason as the original interleave 
in section 3.5.3 – for handling infinite streams. Suppose that stream-flatmap is called on
two streams. The first is infinite, for some (maybe valid) reason. Without using interleave, 
the elements of the second stream won’t be reached, ever.

;; 4.73

The same as 4.72.

;; 4.74

;a)

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
	      (stream-filter (lambda (s)
			       (not (stream-null? s)))
			     stream)))

;b)

This change will not affect the behavior of the system.

;; 4.75

(define (uniquely-asserted query frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((eval-stream
	    (qeval (query-of-unique query)
		   (singleton-stream frame))))
       (if (singleton-stream? eval-stream)
	   eval-stream
	   the-empty-stream)))
   frame-stream))

(define (singleton-stream? stream)
  (and (not (stream-null? stream))
       (stream-null? (stream-cdr stream))))

(define (query-of-unique uniq)
  (car uniq))

(put 'unique 'qeval uniquely-asserted)

(and (supervisor ?person ?boss)
     (unique (supervisor ?anyone ?boss)))

;; 4.76

Here’s merge-frames that merges a pair of frames:

(define (merge-frames f1 f2)
  (if (null f1)
    f2
    (let ((var (caar f1))
          (val (cdar f1)))
      (let ((extension (extend-if-possible var val f2)))
        (if (equal extension 'failed)
          'failed
          (merge-frames (cdr f1) extension))))))

And this is merge-frame-streams that merges two streams of frames, using merge-frames for each pair:

(define (merge-frame-streams s1 s2)
  "Tries to merge each frame of s1 with each frame
  of s2. Returns the stream of successful merges."
  (stream-flatmap
    (lambda (f1)
      (stream-filter
        (lambda (f) (not (equal f 'failed)))
        (stream-map
          (lambda (f2)
            (merge-frames f1 f2))
          s2)))
    s1))

conjoin can then be written thus:

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (merge-frame-streams
      (qeval (first-conjunct conjuncts) frame-stream)
      (conjoin (rest-conjuncts conjuncts) frame-stream))))
