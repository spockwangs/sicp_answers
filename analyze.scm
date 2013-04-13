

(define (list-of-values exps env)
  ;; generate a list of practical parameters of exps
  (if (no-operands? exps)
      '()
      (cons (eval. (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  ;; evaluate `if' expressions
  (if (true? (eval. (if-predicate exp) env))
      (eval. (if-consequent exp) env)
      (eval. (if-alternative exp) env)))

(define (eval-sequence exps env)
  ;; evaluate a sequence of exps in a procedure body or in a
  ;; `begin' expression.
  (cond ((last-exp? exps) (eval. (first-exp exps) env))
	(else (eval. (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  ;; evaluate a assignment expression.
  (set-variable-value! (assignment-variable exp)
		       (eval. (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  ;; evaluate definition expression
  (define-variable! (definition-variable exp)
    (eval. (definition-value exp) env)
    env)
  'ok)
    

;;; Representations of Expressions


;; self-evaluating exps: as a number or string

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

;; variable: as a symbol

(define (variable? exp) (symbol? exp))

;; quatations have the form: (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  ;; 检测表exp是否以tag开始
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
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; 形式参数
		   (cddr exp)))) ; body

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
  ;; 从begin表达式序列中提取要执行的表达式序列
  (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  ;; 把一个序列变换成一个begin表达式
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

;; 过程应用不属于上述任何一种类型的复合表达式

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;; 派生表达式

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
	body
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
   (list 'map map)
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

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (announce-output string)
  (newline) (display string) (newline))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define (analyze exp)
  (cond ((self-evaluating? exp)
	 (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((let? exp) (analyze (let->combination exp)))
	((application? exp) (analyze-application exp))
	(else (error "Unkown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
	  (cproc env)
	  (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
	(error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
			   (map (lambda (aproc) (aproc env))
				aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment (procedure-parameters proc)
			      args
			      (procedure-environment proc))))
	(else
	 (error "Unkown procedure type -- EXECUTE-APPLICATION"
		proc))))


(define (eval-ana. exp env)
  ((analyze exp) env))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval-ana. input the-global-environment)))
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

(define (interpret-ana exp)
  (eval-ana. exp the-global-environment))
