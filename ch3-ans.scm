 
;;; vi:expandtab:tabstop=4:shiftwidth=4:
;;3.1

(define (make-accumulator n)
  (lambda (m)
    (set! n (+ m n))
    n))

;;3.2

(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?)
      count)
    (define (reset-count)
      (set! count 0)
      count)
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
        ((eq? m 'reset-count) (reset-count))
        (else 
         (set! count (+ count 1))
         (f m))))
    mf))

;;3.3 

(define (make-account balance . password)
  (define (withdraw amount)
    (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request -- MAKE-ACCOUNT"
               m))))
  (define (dispatch2 pass m)
    (if (eq? pass (car password))
    (begin (if (eq? m 'test)
           true
           (dispatch m)))
    (begin (if (eq? m 'test)
           false
           (error "Incorrect password")))))
  (if (not (null? password))
    dispatch2
      dispatch))

;;3.4

(define (make-account balance password)
  (let ((count 0))
    (define (withdraw amount)
    ;;; ?0?0??
      (if (>= balance amount)
      (begin (set! balance (- balance amount))
         balance)
      (error "Insufficient funds")))
    (define (deposit amount)
    ;;; ????
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      (display "Calling the police ...")
      (newline))
    (define (dispatch pass m)
      (if (not (eq? pass password))
      (begin 
        (set! count (+ count 1))
        (if (> count 7)
        (call-the-cops)
        (display "Incorrect password!")))
      (cond ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else (error "Unknown request -- MAKE-ACCOUNT"
                 m)))))
    dispatch))

;;3.5

(define (estimate-integral predate x1 y1 x2 y2)
;;; x1 and y1 are the left-bottom corner of a rectangle,
;;; and x2 and y2 are the right-up corner.
  (* (monte-carlo 10000.0 predate)
     (* (- x2 x1) (- y2 y1))))

;;3.6

(define rand-update
  (lambda (x)
    (remainder (+ (* 2 x) 7) 10)))
(define rand
  (let ((x 1))
    (lambda (string)
       (cond ((eq? string 'generate)
          (set! x (rand-update x))
          x)
         ((eq? string 'reset)
          (lambda (reset-value)
            (set! x reset-value)
            reset-value))))))
    
;;3.7

(define make-joint
  (lambda (account newpass oldpass)
    ;;; make-joint ?????0?5??????????0?2?account????0?1?
    ;;; ??0?2???newpass?0?2??0?5?????0?2?????0?3oldpass?0?2account
    ;;; ??????0?6????????0?2???????make-account????????0?2??????0?5????
    (if (account oldpass 'test)
    (lambda (pass m)
      (if (eq? pass newpass)
          (account oldpass m)
          (error "Incorrect password!")))
    (error "Incorrect account or password!"))))
    
;;3.8

(define f
  (let ((temp 0))
    (lambda (a)
      (if (= temp 0)
      (if (<= a 0)
          -1
          (begin (set! temp 1)
             1))
      (if (<= a 0)
          0
          1)))))

;;3.13

dead circle

;;3.14

"mystery" is intended to reverse the list.

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

v => (a)
w => (d c b a)

;;3.17

(define (count-pairs-3.17 x)
  (define (element-of-list? atom list)
    (if (null? list)
    #f
    (if (eq? atom (car list))
        #t
        (element-of-list? atom (cdr list)))))
  (define (add-to-list atom l)
    (cons atom l))
  (let ((track '()))
    (define (iter x)
      (if (not (pair? x))
      0
      (if (element-of-list? x track)
          0
          (begin
        (set! track (add-to-list x track))
        (+ (iter (car x))
           (iter (cdr x))
           1)))))
    (iter x)))
      
;;3.18

(define (have-circle? list)
  (define (iter p1 p2)
    (if (eq? p1 p2)
    #t
    (let ((p1 (cdr p1)))
      (if (not (null? p1))
          (if (not (null? (cddr p2)))
          (let ((p2 (cddr p2)))
            (iter p1 p2))
          #f)
          #f))))
  (if (null? (cdr list))
      #f
      (iter list (cdr list))))
    
;;3.19

(define (check-circle list)
  (define (iter p1 p2)
    (cond ((or (null? p1)
           (null? p2))
       #f)
      ((eq? p1 p2) #t)
      (else
       (iter (cdr p1)
         (cdddr p2)))))
  (if (not (pair? list))
      #f
      (iter list (cdr list))))

;;3.21

(define (print-queue-3.21 queue)
  (display (car queue)))

;;3.22

(define (make-queue-3.22)
  (let ((front-ptr '())
    (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
      (error "FRONT called with an empty queue" front-ptr)
      (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
    (cond ((empty-queue?)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)
           front-ptr)
          (else
           (set-cdr! rear-ptr new-pair)
           (set-rear-ptr! new-pair)
           front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
         (error "DELETE! called with an empty queue" front-ptr))
        (else
         (set-front-ptr! (cdr front-ptr))
         front-ptr)))
    (define (print-queue)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
        ((eq? m 'rear-ptr) rear-ptr)
        ((eq? m 'set-front-ptr!) set-front-ptr!)
        ((eq? m 'set-rear-ptr!) set-rear-ptr!)
        ((eq? m 'empty-queue?) empty-queue?)
        ((eq? m 'front-queue) front-queue)
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) delete-queue!)
        ((eq? m 'print) print-queue)
        (else
         (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

(define (front-ptr-3.22 queue)
  (queue 'front-ptr))

(define (rear-ptr-3.22 queue)
  (queue 'rear-ptr))

(define (set-front-ptr!-3.22 queue item)
  ((queue 'set-front-ptr!) item))

(define (set-rear-ptr!-3.22 queue item)
  ((queue 'set-rear-ptr!) item))

(define (empty-queue?-3.22 queue)
  ((queue 'empty-queue?)))

(define (front-queue-3.22 queue)
  ((queue 'front-queue)))

(define (insert-queue!-3.22 queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue!-3.22 queue)
  ((queue 'delete-queue!)))

(define (print-queue-3.22 queue)
  ((queue 'print)))

;;3.23

(define (make-deque-3.23)
  (cons '() '()))

(define (empty-deqeue?-3.23 deque)
  (null? (car deque)))

(define (front-deque-3.23 deque)
  (car deque))

(define (rear-deque-3.23 deque)
  (cdr deque))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons-node item '() '())))
    (cond ((empty-deqeue?-3.23 deque)
       (set-front-ptr!-3.23 deque new-pair)
       (set-rear-ptr!-3.23 deque new-pair))
      (else
       (set-previous! (front-deque-3.23 deque) new-pair)
       (set-next! new-pair (front-deque-3.23 deque))
       (set-front-ptr!-3.23 deque new-pair)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons-node item '() '())))
    (cond ((empty-deqeue?-3.23 deque)
       (set-front-ptr!-3.23 deque new-pair)
       (set-rear-ptr!-3.23 deque new-pair))
      (else
       (set-next! (rear-deque-3.23 deque) new-pair)
       (set-previous! new-pair (rear-deque-3.23 deque))
       (set-rear-ptr!-3.23 deque new-pair)))))
       
(define (front-delete-deque! deque)
  (cond ((empty-deqeue?-3.23 deque)
     (error "FRONT-DELETE called with an empty deque" deque))
    (else
     (set-front-ptr!-3.23 deque (next-node (front-deque-3.23 deque)))
     (if (empty-deqeue?-3.23 deque)
         (set-rear-ptr!-3.23 deque '())
         (set-previous! (front-deque-3.23 deque) '())))))
    
(define (rear-delete-deque! deque)
  (cond ((empty-deqeue?-3.23 deque)
     (error "REAR-DELETE called with an empty deque" deque))
    (else
     (set-rear-ptr!-3.23 deque (pre-node (rear-deque-3.23 deque)))
     (if (empty-deqeue?-3.23 deque)
         (set-front-ptr!-3.23 deque '())
         (set-next! (rear-deque-3.23 deque) '())))))

(define (print-deque deque)
  (define (iter ptr result)
    (if (null? ptr)
    result
    (iter (next-node ptr) (append result (cons (item ptr) '())))))
  (iter (front-deque-3.23 deque) '()))

(define (set-front-ptr!-3.23 deque node)
  (set-car! deque node))

(define (set-rear-ptr!-3.23 deque node)
  (set-cdr! deque node))

(define (cons-node item pre next)
  (cons item (cons pre next)))

(define (item node)
  (car node))

(define (pre-node node)
  (cadr node))

(define (next-node node)
  (cddr node))

(define (set-previous! node pre)
  (set-car! (cdr node) pre))

(define (set-next! node next)
  (set-cdr! (cdr node) next))

;;3.27

因为memo-fib不会计算同一个结果两次，所以计算第n个Fibonacci数只需要n步。
如果将mem-fib定义为(memoize fib)，这一模式将不能工作。因为fib调用的是fib而不是memo-fib。

;;3.28

(define (logical-or in1 in2)
  (cond ((or (= in1 1) (= in2 1))
     1)
    ((and (= in1 0) (= in2 0))
     0)
    (else (error "Invalid signals" in1 in2))))

(define (or-gate-3.28 a1 a2 output)
  (define (or-action-proc)
    (let ((new-value
        (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
           (lambda ()
             (set-signal! output new-value)))))
  (add-action! a1 or-action-proc)
  (add-action! a2 or-action-proc)
  'ok)

;;3.29

(define (or-gate-3.29 a1 a2 output)
  (let ((t (make-wire)))
    (and-gate a1 a2 t)
    (inverter t output)))

这样定义的或门的延时是and-gate-delay+or-gate-delay

;;3.31

如果不这样做，输出线路的初始值可能是错的。

;;3.32

如果待处理表采用后进先出的顺序，可能会出现错误的值。
考虑一个与门，输入由0, 1变为1, 0, 在这个过程中实际上还有一个短暂的中间过程就是0,1-1,1-1,0, 所以输出值
有一个短暂的1然后马上变成0。若待处理表采用后进先出，则输出值先是变成0，马上变成错误的1.

;;3.33

(define (averager a b c)
  (let ((u (make-connector))
    (v (make-connector)))
    (adder a b u)
    (constant 0.5 v)
    (multiplier u v c)
    'ok))

;;3.34

给b一个值不能算出它的平方根，因为在乘法器看来，两个a都没有值，约束无法传播，因此平方根无法计算。
只能已知a算平方，而不能倒过来。

;;3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
    (if (< (get-value b) 0)
        (error "square less than 0 -- SQUARER"
           (get-value b))
        (set-value! a (sqrt (get-value b)) me))
    (if (has-value? a)
        (set-value! b (square (get-value a))
            me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
       (error "Unkown request -- SQURER" request))))
  (connect a me)
  (connect b me)
  me)

;;3.37

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv value)
  (let ((z (make-connector)))
    (constant value z)
    z))

;;3.39

The possible results are
101: P1 completes then P2 completes
100: P1 computes (* x x) then P2 completes, then P1 assign 100 to x
121: P2 completes(x is 11), then P1 completes.

;;3.40

Without serialization the possible results are: 10^2, 10^3, 10^4, 10^5, 10^6.
With serialization there is only one result: 10^6.

;;3.41

若不对访余额操作串行化，有可能访问到的余额是某个操作的中间值，而这个中间值并不是我们想看到的。

;;3.42

修改以后也是安全的。在并发性方面，两个版本没有什么不同。

;;3.44

我赞成Ben的说法。转移问题与交换问题的不同点在于不需要计算差额。

;;3.45

When serialized-exchange is called, the serializers of both accounts are
activated. Then, when exchange tries to call deposit or withdraw, it can’t
because these functions also try to use the serializer. It will block on the
call of withdraw from account1 and stay in this state indefinitely.

;;3.47

;a)

(define (make-semaphore n)
  (let ((count n)
    (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
         (mutex 'acquire)
         (set! count (- count 1))
         (if (< count 0)
           (begin 
         (mutex 'release)
         (the-semaphore 'acquire)))
         (mutex 'release))
        ((eq? m 'release)
         (mutex 'acquire)
         (set! count (+ count 1))
         (mutex 'release))
        (else
          (error "No such operation -- MAKE-SEMAPHORE" m))))
    the-semaphore))

;b)

(define (make-semaphore n)
  (let ((count n)
    (cell (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
         (if (test-and-set! cell)
           (the-semaphore 'acquire))
         (set! count (- count 1))
         (if (< count 0)
           (begin
         (clear! cell)
             (the-semaphore 'acquire)))
         (clear! cell))
        ((eq? m 'release)
         (if (test-and-set! cell)
           (the-semaphore 'release))
         (set! count (+ count 1))
         (clear! cell))
        (else
          (error "No such operation -- MAKE-SEMAPHORE" m))))
    the-semaphore))

;;3.48

(define (make-account number balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'number) number)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'number) (account2 'number))
      ((serializer2 (serializer1 exchange))
        account1 account2)
      ((serializer1 (serializer2 exchange))
        account1 account2))))

;; 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
          (cons proc (map stream-cdr argstreams))))))

;; 3.51

(define x (stream-map show (stream-enumerate-interval 0 10)))
==> 0

(stream-ref x 5)
==> 1
    2
    3
    4
    5

(stream-ref 7)
==> 6
    7

这说明delay的实现有记忆。

;; 3.52

;sum =
;   有记忆      无记忆
;     1       1
;     6       6
;     10              15        
;     136         162
;     210         362

对于有记忆的情况:
(stream-ref y 7) ==> 136
(display-stream z) ==>
10
15
45
55
105
120
190
210

对于无记忆的情况：
(stream-ref y 7) ==> 134
(display-stream z) ==>
10
180
230
305

;;3.53

;2^n

;; 3.54

(define factorials (cons-stream 1 (mul-streams factorials integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; 3.55

(define (partial-sums s)
  (cons-stream (stream-car s)
           (add-streams (partial-sums s)
                (stream-cdr s))))

;; 3.56

(define S (cons-stream 1 (merge (scale-stream S 2)
                (merge (scale-stream S 3)
                       (scale-stream S 5)))))

;; 3.57

若不采用记忆，则会重复计算斐波那契数. 因为(stream-cdr fibs)会做同样的事情。

;; 3.58

expand计算num在radix基数下除以den所得的在小数点后的序列.

;; 3.59

;a)
(define (integrate-series s)
  (define (iter s n)
    (cons-stream
      (/ (stream-car s) n)
      (iter (stream-cdr s) (+ n 1))))
  (cons 'c (iter s 1))

;b)
(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream 0 (scale-stream 
           (integrate-series cosine-series)
           -1)))

;; 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
           (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

;; 3.61

(define (reciprocal-series s)
  (cons-stream 1
           (mul-series (scale-stream (stream-cdr s) -1)
                       (reciprocal-series s))))

;; 3.62

(define (div-series s1 s2)
  (let ((c (stream-car s2)))
    (cond ((= c 0)
	   (error "Stream-car of s2 is 0" s2))
	  (else
	   (let ((reci (reciprocal-series (scale-stream s2 (/ 1 c)))))
	     (mul-series (scale-stream s1 (/ 1 c))
			 reci))))))

(define tangent-series (div-series sine-series cosine-series))

;; 3.63

It is less efficient because of involving a recursive call of sqrt-stream, which is 
not optimized by memo-proc.
If we don't make use of the optimized version, the two programs are no difference
in efficiency.

;; 3.64

(define (stream-limit s tolerance)
  (let ((item1 (stream-car s))
	(item2 (stream-car (stream-cdr s))))
    (if (< (abs (- item1 item2)) tolerance)
	item2
	(stream-limit (stream-cdr s) tolerance))))

;; 3.65


(define (ln2-sum n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-sum (+ n 1)))))
(define ln2 (partial-sums (ln2-sum 1)))



;; 3.66

; (n, n)??????????0?7?2^n-1??
; (m, n)??????????0?7?(n-m)*2^n?????0?0??n>m.

;; 3.67

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
         (stream-cdr s))
     (all-pairs (stream-cdr s) (stream-cdr t))))))

;; 3.69

(define (triplets s1 s2 s3)
  (cons-stream
    (list
      (stream-car s1)
      (stream-car s2)
      (stream-car s3))
    (interleave
      (stream-map
        (lambda (x) (append (list (stream-car s1)) x))
        (stream-cdr (pairs s2 s3)))
      (triplets
        (stream-cdr s1)
        (stream-cdr s2)
        (stream-cdr s3)))))

(define ppp (triplets integers integers integers))

(define pythagoren
  (stream-filter (lambda (triplet)
		   (= (square (caddr triplet))
		      (+ (square (car triplet))
			 (square (cadr triplet)))))
		 ppp))


;; 3.70

(define (merge-weighted s1 s2 weight)
      (cond ((stream-null? s1) s2)
            ((stream-null? s2) s1)
            (else
             (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
               (cond ((<= (weight s1car) (weight s2car))
                      (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                     (else
                      (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))


(define (weighted-pairs s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted 
    (stream-map (lambda (x) (list (stream-car s1) x))
                (stream-cdr s2))
    (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
    weight)))

; a

(define (weight p)
  (+ (car p) (cadr p)))

(weighted-pairs integers integers weight)

; b

(define (weight p)
  (let ((i (car p))
    (j (cadr p)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define (predate? p)
  (define (check x)
    (or (= (remainder x 2) 0)
    (= (remainder x 3) 0)
    (= (remainder x 5) 0)))
  (let ((i (car p))
    (j (cadr p)))
    (or (check i)
    (check j))))

(stream-filter predate? (weighted-pairs
             integers
             integers
             weight))

;; 3.71

(define (Ramanujan)
  (define (weight p)
    (let ((i (car p))
      (j (cadr p)))
      (+ (* i i i)
     (* j j j))))
  (define (filter? stream)
    (cond ((stream-null? stream) the-empty-stream)
      ((= (weight (stream-car stream))
          (weight (stream-car (stream-cdr stream))))
       (cons-stream (list (weight (stream-car stream))
                  (stream-car stream)
                  (stream-cadr stream))
            (filter? (stream-cdr stream))))
      (else (filter? (stream-cdr stream)))))
  (filter? (weighted-pairs integers integers weight)))

(display-stream (ramanujan))
==>
(1729 (1 12) (9 10))
(4104 (2 16) (9 15))
(13832 (2 24) (18 20))
(20683 (10 27) (19 24))
(32832 (4 32) (18 30))
(39312 (2 34) (15 33))

;; 3.72

(define (squares-3ways)
  (define (ij-square p)
    (let ((i (car p))
      (j (cadr p)))
      (+ (square i)
     (square j))))
  (define (filter? s)
    (if (stream-null? s)
     the-empty-stream
    (let ((i (stream-car s))
          (j (stream-cadr s))
          (k (stream-cadr (stream-cdr s))))
      (if (= (ij-square i) (ij-square j) (ij-square k))
          (cons-stream (list (ij-square i) i j k)
               (filter? (stream-cdr s)))
          (filter? (stream-cdr s))))))
  (filter? (weighted-pairs integers integers ij-square)))

(display-stream (squares-3ways))
==>
(325 (1 18) (6 17) (10 15))
(425 (5 20) (8 19) (13 16))
(650 (5 25) (11 23) (17 19))
(725 (7 26) (10 25) (14 23))
(845 (2 29) (13 26) (19 22))
(850 (3 29) (11 27) (15 25))
(925 (5 30) (14 27) (21 22))
(1025 (1 32) (8 31) (20 25))
(1105 (4 33) (9 32) (12 31))
      
;; 3.73

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (scale-stream i R)
     (integral (scale-stream i (/ 1.0 C))
           v0
           dt))))

;; 3.74

(define (sign-change-detector a b)
  (cond ((and (< a 0) (>= b 0)) 1)
    ((and (>= a 0) (< b 0)) -1)
    (else 0)))

(define zero-crossings
  (stream-map sign-change-detector
          sense-data
          (stream-cdr sense-data)))

;; 3.75

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (average (stream-car input-stream) last-value)))
    (cons-stream (sign-change-detector last-avpt avpt)
         (make-zero-crossings (stream-cdr input-stream)
                      (stream-car input-stream)
                      avpt))))

;; 3.76

(define (smooth s)
  (cons-stream (average (stream-car s)
            (stream-cadr s))
           (smooth (stream-cdr s))))

(define (make-zero-crossings input-stream last-value smooth)
  (let ((smooth-stream (smooth (cons-stream last-value input-stream))))
    (stream-map3 sign-change-detector
         smooth-stream
         (stream-cdr smooth-stream))))

;; 3.77

(define (integral-delayed delayed-integrand intitial-value dt)
  (cons-stream intitial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral-delayed (delay (stream-cdr integrand))
				       (+ (* dt (stream-car integrand))
					  intitial-value)
				       dt)))))

;; 3.78

(define (solve-2nd a b y0 dy0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (integral-delayed (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
               (scale-stream y b)))
  y)

;; 3.79

(define (solve-2nd-general f y0 dy0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (integral-delayed (delay ddy) dy0 dt))
  (define ddy (stream-map3 f dy y))
  y)

;; 3.80

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral-delayed (delay dvc) vc0 dt))
    (define il (integral-delayed (delay dil) il0 dt))
    (define dvc (scale-stream il (- (/ 1 C))))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                 (scale-stream il (- (/ R L)))))
    (define (compose-stream s1 s2)
      (cons-stream (cons (stream-car s1)
             (stream-car s2))
           (compose-stream (stream-cdr s1)
                   (stream-cdr s2))))
    (compose-stream vc il)))
    
;; 3.81

(define (rand input-stream)
  (define r
    (if (stream-null? input-stream)
	the-empty-stream
	(cons-stream random-init
		     (stream-map
		      (lambda (x y)
			(cond ((eq? x 'generate)
			       (rand-update y))
			      ((eq? x 'reset)
			       random-init)))
		      input-stream r))))
  r)


