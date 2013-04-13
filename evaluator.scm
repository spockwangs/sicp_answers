
(define (eval-wbb exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval-wbb (cond->if exp) env))
	((let? exp) (eval-wbb (let->combination exp) env))
	((application? exp)
	 (apply-wbb (eval-wbb (operator exp) env)
		    (list-of-values (operands exp) env)))
	(else
	 (error "Unkown expressions type -- EVAL" exp))))

(define (apply-wbb procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error "Unkown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  ;; generate a list of practical parameters of exps
  (if (no-operands? exps)
      '()
      (cons (eval-wbb (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  ;; evaluate `if' expressions
  (if (true? (eval-wbb (if-predicate exp) env))
      (eval-wbb (if-consequent exp) env)
      (eval-wbb (if-alternative exp) env)))

(define (eval-sequence exps env)
  ;; evaluate a sequence of exps in a procedure body or in a
  ;; `begin' expression.
  (cond ((last-exp? exps) (eval-wbb (first-exp exps) env))
	(else (eval-wbb (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  ;; evaluate a assignment expression.
  (set-variable-value! (assignment-variable exp)
		       (eval-wbb (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  ;; evaluate definition expression
  (define-variable! (definition-variable exp)
    (eval-wbb (definition-value exp) env)
    env)
  (definition-variable exp))
    

;;; Representations of Expressions

;; self-evaluating exps: as a number or string

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

;; variable: as a symbol

(define (variable? exp) (symbol? exp))

;; quotations have the form: (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  ;; check if `exp' begins with `tag'
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; assignments: (set! <var> <value>)

(define (make-assignment var value)
  (list 'set! var value))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


;; definition: (define <var> <value>)
;;             (define (<var> <parameter1> ... <parametern>)
;;                     <body>)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (pair? exp)
      (let ((rest (cdr exp)))
	(cond ((null? rest)
	       (error "Ill-formed special form: " exp))
	      ((symbol? (car rest))
	       (car rest))
	      (else (caar rest))))))

(define (definition-value exp)
  (if (null? (cddr exp))
      (error "Ill-formed special form: " exp)
      (if (symbol? (cadr exp))
	  (caddr exp)
	  (make-lambda (cdadr exp)   ; formal parameters
		       (cddr exp))))) ; body

(define (make-define var parameter . body)
  (list 'define
	var
	(make-lambda parameter body)))

;; lambda expressions

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  ;; construct a lambda expression, used in `definition-value'
  (cons 'lambda (cons parameters body)))

;; if expressions

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  ;; construct a if expression, used in `cond->if'
  (list 'if predicate consequent alternative))

;; begin expressions

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp)
  ;; extract the begin executable sequence
  (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  ;; turn a sequence to a begin expression
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

;; procedural application 

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;; derived expressions

;; cond expressions

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;; let expression

(define (let? exp) (tagged-list? exp 'let))

(define (let-clause exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-variables exp)
  (map car (cadr exp)))

(define (let-exps exp)
  (map cadr (cadr exp)))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp)
		     (let-body exp))
	(let-exps exp)))

(define (make-let var-list let-body)
  (cons 'let (cons var-list let-body)))
		  
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

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

(define (make-procedure parameters body env)
  (list 'procedure
	parameters
	(scan-out-defines body)
	env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame vars vals)
  (cons vars vals))
;(define (make-frame vars vals)
;  (if (list? vars)
;      (cons vars vals)
;      (cond ((not (pair? vars))
;	 (cons (cons vars '())
;	       (cons vals '())))
;	((pair? (cdr vars))
;	 (process (cdr vars)
;		  (cdr vals)))
;	(else
;	 (set-cdr! vars (cons (cdr vars) '()))
;	 (set-cdr! vals (cons (cdr vals) '()))
;	 (cons vars vals)))))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (list? vars)
      (if (= (length vars) (length vals))
	  (cons (make-frame vars vals) base-env)
	  (if (< (length vars) (length vals))
	      (error "Too many arguments supplied" vars vals)
	      (error "Too few arguments supplied" vars vals)))
      (cons (make-frame vars vals) base-env)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((pair? vars)
	     (if (eq? var (car vars))
		 (if (eq? (car vals) '*unassigned*)  ; check if the value is equal to *unassigned*?
		     (error "Premature reference to reserved name: " var)
		     (car vals))
		 (scan (cdr vars) (cdr vals))))
	    ((eq? var vars)
	     vals)
	    (else (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable:" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define primitive-procedures
  (list
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? null?)
   (list '+ +)
   (list '- -)
   (list '= =)
   (list '< <)
   (list '> >)
   (list '<= <=)
   (list '>= >=)
   (list '* *)
   (list 'not not)
   (list 'list list)
   (list 'member member)
   (list 'memq memq)
   (list 'eq? eq?)
   (list 'equal? equal?)
   ))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define input-prompt ">> ")

(define output-prompt ";Value: ")

(define (announce-output string)
  (display string))

(define (prompt-for-input string)
  (newline) (newline) (display string))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval-wbb input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (interpret exp)
  (eval-wbb exp the-global-environment))
