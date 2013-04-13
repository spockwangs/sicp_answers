;;1.1

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (>  b a) (< b (* a b)))
  b
  a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;;1.2

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5))))))
;Value: 5/74

;;1.3

(define (max-plus a b c)
  (if (> a b)
    (if (> b c)
      (+ a b)
      (+ a c))
    (if (> a c)
      (+ b a)
      (+ b c))))

;;1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;表示a加上b的绝对值

;;1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))
(test 0 (p))
;如果解释器采用的是 applicative order，解释器将进入死循环。如果采用的是 normal order，返回0。

;;1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;解释器会陷入死循环，最终用完栈空间而退出。

;;1.7

(define good-enough?
  (lambda (guess x)
    (< (/ (abs (- guess (improve guess x)))
          guess)
       0.001)))

对很大和很小的数都能工作。

;;1.8

(define (subtriplicate x)
  (define (improve guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (good-enough? guess x)
    (< (/ (abs (- guess (improve guess x)))
          guess)
       0.0001))
  (define (subtriplicate-iter guess x)
    (if (good-enough? guess x)
      guess
      (subtriplicate-iter (improve guess x)
                          x)))
  (subtriplicate-iter 1.0 x))

;;1.9

(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
; 递归过程

(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))
; 迭代过程

;;1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;Value: 1024

(A 2 4)
;Value: 65536

(A 3 3)
;Value: 65536

(define (f n) (A 0 n))
;compute 2*n

(define (g n) (A 1 n))
;compute 2**n

(define (h n) (A 2 n))
;compute 2**h(n-1)

;;1.11

; recursive version
(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f (- n 1))
                     (* 2 (f (- n 2)))
                     (* 3 (f (- n 3)))))))

; iterative version
(define (f n)
  (define (f-iter a b c count)
    (if (= count 0)
      a
      (f-iter b 
              c
              (+ c (* 2 b) (* 3 a)) 
              (- count 1))))
  (f-iter 0 1 2))

;;1.12

(define (pascal n)
  (define (pascal-iter seq result)
    (if (null? (cdr seq))
      result
      (pascal-iter (cdr seq)
                   (append result (list (+ (car seq)
                                           (cadr seq)))))))
  (cond ((= n 1) (list 1 1))
        (else (append (pascal-iter (pascal (- n 1))
                                   (list 1))
                      '(1)))))

;;1.14
时间复杂度是theta(n^3), 空间复杂度是theta(n)

;;1.15

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

; a)
(sine 12.15)
(p (sine 4.5))
(p (p (sine 1.5)))
(p (p (p (sine 0.5))))
(p (p (p (p (sine 0.1666666666)))))
(p (p (p (p (p (sine 0.0555555555))))))
; so p is called 5 times

; b)
; 每当a增长3倍时p就被调一次，因此空间及时间复杂度均为theta(log3(n))

;;1.16

(define (expt b n)
  (define (expt-iter b n result)
    (cond ((= n 0) 1)
          ((= n 1) (* b result))
          ((odd? n) (expt-iter b (- n 1) (* b result)))
          (else (expt-iter (square b) (/ n 2) result))))
  (expt-iter b n 1))

;;1.17

(define (fast-times a b)
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-times a (halve b))))
        (else (+ a (fast-times a (- b 1))))))

;;1.18

(define (times a b)
  (define (times-iter a b result)
    (cond ((= b 0) result)
          ((odd? b) (times-iter a (- b 1) (+ a result)))
          (else (times-iter (double a) (halve b) result))))
  (times-iter a b 0))

;;1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))  ; compute p'
                   (+ (* 2 p q) (square q))   ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;1.20
With normal order, it's 18 times.
With applicative order, it's 4 times.

;;1.21

(smallest-divisor 199)
;Value: 199

(smallest-divisor 1999)
;Value: 1999

(smallest-divisor 19999)
;Value: 7

;;1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes low high)
  (if (<= (low high))
      (begin
	(timed-prime-test low)
	(search-for-primes (+ low 1) high))))
	  
;;1.23

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

;;1.24

(define (fast-timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (fast-search-for-primes low high)
  (define (prime-test-iter count)
    (if (and (< count high)
             (prime? count))
      (fast-timed-prime-test count)
      (prime-test-iter (+ count 1))))
  (prime-test-iter low))

;;1.27

(define (carmichael-test n)
  (define (iter count)
    (if (< count n)
      (if (= (expmod count n n) count)
        (iter (+ count 1))
        false)
      true))
  (if (not (prime? n))
    (iter n)
    false))

;;1.28

(define (miller-rabin-test n)
  (define (try-it a)
    (and (= (expmod a n n) a)
         (if (and (< a (- n 1))
                  (> a 1))
           (not (= (remainder (square a) n) 1))
           true)))
  (try-it (+ 1 (random (- n 1)))))

(define (new-fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n)
	 (new-fast-prime? n (- times 1)))
        (else false)))

;;1.29

(define (simpson-integrate f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (let ((tmp (f (+ a (* h k)))))
      (cond ((or (= k 0)
                 (= n k))
             tmp)
            ((even? k) (* 2 tmp))
            (else (* 4 tmp)))))
  (define (next k)
    (+ k 1))
  (/ (* (sum term 0 next n) h)
     3.0))

;;1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

;;1.31

;; recursive version
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

;; iterative version
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (term k)
    k)
  (define (next k)
    (+ k 1))
  (product term 1 next n))

(define (pi)
  (define (term k)
    (/ (* (* 2.0 k)
          (+ 2.0 (* 2.0 k)))
       (square (+ 1.0 (* 2.0 k)))))
  (define (next k)
    (+ 1 k))
  (* 4 (product term 1 next 1000000)))

;;1.32

; recursive version
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
             (accumulate combiner null-value term (next a) next b))))

; iterative version
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;1.33

(define (filtered-accumulate combiner null-value term a next b filter?)
  (if (> a b)
    null-value
    (if (filter? (term a))
      (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter?))
      (filtered-accumulate combiner null-value term (next a) next b filter?))))
; a)

(define (identity x) x)

(define (next x)
  (if (even? x)
    (+ x 1)
    (+ x 2)))

(filtered-accumulate + 0 identity 1 next 20 prime?)

; b)

(define (filtered-acc n)
  (define (identity x) x)
  (define (next x)
    (+ x 1))
  (define (filter? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 1 next n filter?))

;;1.34

(define (f g)
  (g 2))

(f f)
; 出现错误

;;1.35

(define golden-value 
  (fixed-point (lambda (x) (+ 1 
                              (/ 1 x)))
               1.0))

;;1.36

(define (new-fixed-point f first-guess)
   (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
   (define (try guess)
     (let ((next (f guess)))
       (newline)
       (display guess)
       (if (close-enough? guess next)
           next
           (try next))))
   (try first-guess))

(new-fixed-point (lambda (x)
                   (/ (log 1000) (log x)))
                 1.1)
(new-fixed-point (lambda (x)
                   (/ (+ x
                         (/ (log 1000) (log x)))
                      2))
                 1.1)
;;1.37

; recursive version
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i)
         (+ (d i)
            (iter (+ i 1))))))
  (iter 1))

; iterative version
(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
      result
      (iter (- k 1)
            (/ (n k)
               (+ (d k)
                  result)))))
  (iter k 0))

;;1.38

(define (n i) 1.0)
(define (d i)
  (if (= (remainder i 3) 2)
    (* 2.0
       (+ 1
          (/ (- i (remainder i 3))
             3)))
    1.0))
(define e (cont-frac n d 1000))

;;1.39

(define (tan-cf x k)
  (define (n i x)
    (if (= i 1)
        x
        (* x x)))
  (define (d i)
    (- (* 2 i)
       1))
  (define (iter k result)
    (if (= k 0)
      result
      (iter (- k 1)
            (/ (n k x)
               (- (d k)
                  result)))))
  (iter k 0))

;;1.40

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;;1.41

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc i)
  (+ i 1))

((double inc) 2)

(((double double) inc) 5)
==> 9
(((double (double double)) inc) 5)
==> 21

;;1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;1.43

(define (repeated f n)
  (lambda (x)
    (cond ((= n 1) (f x))
          ((even? n)
           ((repeated (compose f f) (/ n 2)) x))
          (else ((repeated f (- n 1)) (f x))))))
;;1.44

(define dx 1.0)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-n f n)
  (lambda (x)
    ((repeated (smooth f) n) x)))

;;1.45

(define (sqrt-n x n m)
;; n: 求n次方根
;; m: 求平均阻尼的次数
  (define (f x n)
    (lambda (y)
      (/ x
         (expt y (- n 1)))))
  (fixed-point (repeated (average-damp (f x n))
                         m)
               1.0))

2 1
3 1
4 2
5 2
6 1
7 1
8 1
9 1
10 1
11 1
12 1
13 4
14 1
15 1
16 1

;;1.46

(define (iterative-improve close-enough? improve)
  (lambda (x-or-fun guess)
    (if (close-enough? guess x-or-fun)
        guess
        ((iterative-improve close-enough? improve)
         x-or-fun
         (improve guess x-or-fun)))))

(define (sqrt x)
  (define (close-enough? guess x)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess x)
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) x 1.0))

(define (fixed-point f guess)
  (define (close-enough? guess x)
    (< (abs (- guess
               (x guess)))
       tolerance))
  (define (improve guess x)
    (x guess))
  ((iterative-improve close-enough? improve)
   f guess))
