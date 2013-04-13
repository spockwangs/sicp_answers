;;2.1

(define (make-rat n d)
  (cond ((and (< n 0) (< d 0))
	 (let ((g (gcd (- n) (- d))))
	   (cons (/ (- n) g) (/ (- d) g))))
	((and (>= n 0) (< d 0)
	 (let ((g (gcd n (- d))))
	   (cons (/ (- n) g) (/ (- d) g)))))
	((and (< n 0) (> d 0))
	 (let ((g (gcd (- n) d)))
	   (cons (/ n  g) (/ d g))))
	((and (>= n 0) (> 0))
	 (let ((g (gcd n d)))
	   (cons (/ n g) (/ d g))))
	((= d 0)
	 (error " Denominator can not be 0 -- MAKE-RAT "))))
   

;;2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment point1 point2)
  (cons point1 point2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (midpoint-segment segment)
  (let ((start-point (start-segment segment))
	(end-point (end-segment segment)))
    (make-point
     (average (x-point start-point)
	      (x-point end-point))
     (average (y-point start-point)
	      (y-point end-point)))))

;;2.3

(define (make-rect point1 point2)
  (cons point1 point2))

(define (left-upper rectangle)
  (car rectangle))

(define (right-bottom rectangle)
  (cdr rectangle))

(define (make-rect point1 point2)
  (cons point1
	(make-point
	 (abs (- (x-point point1)
		 (x-point point2)))
	 (abs (- (y-point point1)
		 (y-point point2))))))

(define (left-upper rectangle)
  (car rectangle))

(define (right-bottom rectangle)
  (make-point
   (+ (x-point (left-upper rectangle))
      (x-point (cdr rectangle)))
   (+ (y-point (left-upper rectangle))
      (y-point (cdr rectangle)))))

(define (rect-area rectangle)
  (let ((width (abs (- (x-point (left-upper rectangle))
		       (x-point (right-bottom rectangle)))))
	(height (abs (- (y-point (left-upper rectangle))
			(y-point (right-bottom rectangle))))))
    (* width height)))

(define (rect-circumference rectangle)
  (let ((width (abs (- (x-point (left-upper rectangle))
		       (x-point (right-bottom rectangle)))))
	(height (abs (- (y-point (left-upper rectangle))
			(y-point (right-bottom rectangle))))))
    (+ (* 2 width) (* 2 height))))

;;2.4

(define (cdr z)
  (z (lambda (q p) p)))

;;2.5

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (define (iter z result)
    (if (= (remainder z 2) 0)
      (iter (/ z 2) (+ result 1))
      result))
  (iter z 0))
(define (cdr z)
  (define (iter z result)
    (if (= (remainder z 3) 0)
      (iter (/ z 3) (+ result 1))
      result))
  (iter z 0))

;;2.6

(define zero
  (lambda (f) (lambda (x) x)))

(define one
  (lambda (f) (lambda (x) (f x))))
(define two 
  (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
  (lambda (f) (lambda (x)
		((a f) ((b f) x)))))

;;2.7

(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

;;2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;;2.10

(define (div-interval x y)
  (if (and (< (lower-bound y) 0)
	   (> (upper-bound y) 0))
    (error "Interval " y "includes 0")
    (mul-interval x
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))

;;2.12

(define (make-center-percent c p)
  (make-interval (- c (* p c))
		 (+ c (* p c))))
(define (percent i)
  (/ (width i) (center i)))

;;2.17 

(define (last-pair l)
  (if (null? l)
      '()
      (if (null? (cdr l))
	  l
	  (last-pair (cdr l)))))

;;2.18

(define (reverse l)
  (if (null? l)
      '()
      (if (null? (cdr l))
	  l
	  (append (reverse (cdr l)) (list (car l))))))

;;2.19

(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

;;2.20

(define (same-parity x . y)
  (define (iter l predate result)
    (if (null? l)
      result
      (if (predate (car l))
	(iter (cdr l) predate (append result (list (car l))))
	(iter (cdr l) predate result))))
  (if (even? x)
    (iter y even? (list x))
    (iter y odd? (list x))))

;;2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))

;;2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
   (iter items nil))

;;2.23

(define (for-each proc terms)
  (if (not (null? terms))
      (begin (proc (car terms))
	     (for-each proc (cdr terms)))))

;;2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;Value: (1 2 3 4 5 6)

(cons x y)
;Value: ((1 2 3) 4 5 6)

(list x y)
;Value: ((1 2 3) (4 5 6))

;;2.27

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse l)
  (cond ((null? l) nil)
	((not (pair? l)) l)
	(else (list (deep-reverse (cadr l))
		    (deep-reverse (car l))))))

;;2.28

(define (fringe term)
  (cond ((null? term) nil)
	((not (pair? term)) (cons term nil))
	(else (append (fringe (car term))
		      (fringe (cdr term))))))

;;2.29

; a)

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))


; b)

(define (total-weight mobile)
  (define (weight branch)
    (cond ((null? branch ) 0)
	  ((pair? (branch-structure branch))
	   (total-weight (branch-structure branch)))
	  (else (branch-structure branch))))
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

; c)

(define (balance? mobile)
  (cond ((null? mobile) true)
	(else (and (= (* (branch-length (left-branch mobile))
			 (weight (left-branch mobile)))
		      (* (branch-length (right-branch mobile))
			 (weight (right-branch mobile))))
		   (if (pair? (branch-structure (left-branch mobile)))
		       (balance? (branch-structure (left-branch mobile)))
		       true)
		   (if (pair? (branch-structure (right-branch mobile)))
		       (balance? (branch-structure (right-branch mobile)))
		       true)))))

; d)
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-structure branch)
  (cdr branch))

;;2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (square-tree sub-tree)
	   (square sub-tree)))
       tree))

;;2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (tree-map proc sub-tree)
	   (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

;;2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
			    (cons (car s) x))
			  rest)))))


;;2.33

(define (map p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      nil
	      sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0
	      sequence))

;;2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
		(+ this-coeff
		   (* x higher-terms)))
              0
              coefficient-sequence))

;;2.35

(define (count-leaves-new t)
  (if (not (pair? t))
      1
      (accumulate +
		  0
		  (map (lambda (x)
			 (cond ((null? x) 0)
			       ((not (pair? x)) 1)
			       (else (+ (count-leaves-new (car x))
					(count-leaves-new (cdr x))))))
		       t))))

;;2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;2.37

(define (matrix-*-vector m v)
  (map (lambda (w)
	 (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w)
	   (matrix-*-vector cols w))
	 m)))

;;2.39

(define (reverse sequence)
  (fold-right (lambda (x y)
		(append y (cons x nil)))
	      nil
	      sequence))

(define (reverse sequence)
  (fold-left (lambda (x y)
	       (cons y x))
	     nil
	     sequence))

;;2.40

(define (unique-pairs n)
  (accumulate append nil
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 (- n 1)))))

(define (prim-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;2.41

(define (unique-triplets n)
  (accumulate append nil
	      (map (lambda (k)
		     (accumulate append nil
				 (map (lambda (j)
					(map (lambda (i)
					       (list i j k))
					     (enumerate-interval 1 (- j 1))))
				      (enumerate-interval 1 (- k 1)))))
		   (enumerate-interval 1 n))))

(define (add-to-s s n)
  (define (predate-2.41 triplet)
    (= s
       (+ (car triplet)
	  (cadr triplet)
	  (caddr triplet))))
  (filter predate-2.41 (unique-triplets n)))

;;2.42

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (define (iter counter queens)
    (cond ((or (= counter k)
	       (null? queens))
	   (append rest-of-queens (cons new-row nil)))
	  ((= new-row (car queens)) nil)
	  (else (iter (+ counter 1)
		      (cdr queens)))))
  (iter 1 rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (define (iter counter pos)
    (cond ((= counter k) true)
	  ((or (= (+ (car pos)
		     (- k counter))
		  end-pos)
	       (= (- (car pos)
		     (- k counter))
		  end-pos))
	   false)
	  (else (iter (+ 1 counter)
		      (cdr pos)))))
  (define (iter-pos l)
    (if (not (null? l))
	(if (null? (cdr l))
	    (car l)
	    (iter-pos (cdr l)))))
  (define end-pos
    (iter-pos positions))
  (if (null? positions)
      false
      (if (= (length positions) 1)
	  true
	  (iter 1 positions))))
  

;;2.44

(define (upsplit painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;;2.45

(define (split proc1 proc2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split proc1 proc2) painter (- n 1))))
	(proc1 painter
	       (proc2 smaller smaller))))))

;;2.46

(define (make-vect x y)
  (cons x y))
(define (xcor-vect x y)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

;;2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (origin frame)
  (car frame))
(define (edge1 frame)
  (caadr frame))
(define (edge2 frame)
  (cdadr frame))

;;2.48

(define (make-segment v1 v2)
  (list v1 v2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))

;;2.49

; a)

(define painter1
  (lambda (frame)
    (let ((x (edge1-frame frame))
	  (y (edge2-frame frame)))
      (segments->painter
	((0 0) (0 y))
	((0 0) (x 0))
	((x 0) (x y))
	((0 y) (x y))))))

;;2.50

(define (flip-horiz painter)
  (transformer-painter painter
		       (make-vect 1.0 0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transformer-painter painter
		       (make-vect 1.0 1.0)
		       (make-vect 0 1.0)
		       (make-vect 1.0 0)))

(define (rotate270 painter)
  (transformer-painter painter
		       (make-vect 0 1.0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)))

;;2.51

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
		    (rotate270 painter2))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
	    (transformer-painter painter1
				 (make-vect 0.0 0.0)
				 split-point
				 (make-vect 1.0 0.0)))
	  (paint-up
	    (transformer-painter painter2
				 (make-vect 1.0 0.5)
				 (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-below frame)
	(paint-up frame)))))

;;2.54

(define (equal? a b)
  (if (pair? a)
    (if (pair? b)
      (and (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))
      false)
    (if (not (pair? b))
      (eq? a b)
      false)))
;;2.55

(car ''abracadabra)
==> (car (quote (quote abracadabra)))

;;2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product
	   (exponent exp)
	   (make-exponentiation 
	    (base exp)
	    (- (exponent exp) 1)))
	  (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? exp)
  (if (pair? exp)
    (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	(else (list '** base exponent))))

;;2.57

(define (augend s)
  (if (= (length s) 3)
    (caddr s)
    (cons '+ (cddr s))))

(define (multiplicand p)
  (if (= (length p) 3)
    (caddr p)
    (cons '* (cddr p))))

;;2.58

; a)

(define (sum? e)
  (and (pair? e)
       (eq? (cadr e) '+)))
(define (addend e)
  (car e))
(define (augend e)
  (caddr e))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (product? e)
  (and (pair? e)
       (eq? (cadr e) '*)))
(define (multiplier p)
  (car p))
(define (multiplicand p)
  (caddr p))
(define (make-product v1 v2)
  (cond ((or (=number? v1 0) (=number? v2 0)) 0)
        ((=number? v1 1) v2)
        ((=number? v2 1) v1)
        ((and (number? v1) (number? v2)) (* v1 v2))
	(else (list v1 '* v2))))

; b)

(define (augend exp)
  (if (> (length exp) 3)
      (cddr exp)
      (caddr exp)))
(define (multiplicand exp)
  (if (> (length exp) 3)
      (cddr exp)
      (caddr exp)))

;;2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1)
		    (union-set (cdr set1) set2)))))

;;2.60

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set2)
  (cons x set2))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;;2.61

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< x (car set)) (cons x set))
	((= x (car set)) set)
	(else
	 (cons (car set)
	       (adjoin-set x (cdr set))))))

;;2.62

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (union-set (cdr set1)
				(cdr set2))))
	      ((< x1 x2)
	       (cons x1 (union-set (cdr set1)
				   set2)))
	      ((> x1 x2)
	       (cons x2 (union-set set1
				   (cdr set2))))))))

;;2.63

;a)

The result is the same.

;b)

The time complexity of the first one is theta(nlogn).
Because the oder of growth for append is theta(n).

The second one is theta(n).

;;2.64

;b)
theta(n)

;;2.65

(define (new-union-set set1 set2)
  (list->tree (union-set (tree->list-2 set1)
			 (tree->list-2 set2))))

(define (new-intersection-set set1 set2)
  (list->tree (intersection-set (tree->list-2 set1)
				(tree->list-2 set2))))

;;2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records)))
	 (lookup given-key (right-branch set-of-records)))))
 
;;2.67

(define (lookup-tree given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup-tree given-key
		      (left-branch set-of-records)))
	((> given-key (key (entry set-of-records)))
	 (lookup-tree given-key
		      (right-branch set-of-records)))))

;;2.68

(define (encode-symbol char tree)
  (define (element-of-set? char tree)
    (if (leaf? tree)
	(eq? char (cadr tree))
	(if (memq char (caddr tree))
	    true
	    false)))
  (cond ((leaf? tree) '())
	((element-of-set? char (left-branch tree))
	 (cons 0 (encode-symbol char (left-branch tree))))
	((element-of-set? char (right-branch tree))
	 (cons 1 (encode-symbol char (right-branch tree))))
	(else (error " not in the tree -- ENCODE-SYMBOL"))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;;2.69

(define (successive-merge leaf-set)
  (define (iter-merge result)
    (cond ((= (length result) 1) (car result))
	  (else (iter-merge
		  (adjoin-set (make-code-tree
				(car result) (cadr result))
			      (cddr result))))))
  (iter-merge leaf-set))

;;2.70

(define huffman-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2)
						    (YIP 9) (JOB 2) (WAH 1))))

(length (encode '(Get a job
		      Sha na na na na na na na na
		      Get a job
		      Sha na na na na na na na na
		      Wah yip yip yip yip yip yip yip yip yip
		      Sha boom) huffman-tree))
==> 84

Using Huffman coding needs 84 bits. Using fixed length coding it needs 272 bits.

;;2.71
The most frequent symbol needs 1 bit of coding.
The least frequent symbol needs n-1 bits of coding.

;;2.72

The most frequent symbol needs theta(1).
The least frequent symbol needs theta(n^2).

;;2.73

;b)
(define (deriv-product exp var)
  (make-sum
   (make-product (multiplier exp)
		 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
		 (multiplicand exp))))
(put 'deriv '* deriv-product)
(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
	    (deriv (augend exp) var)))
(put 'deriv '+ deriv-sum)

;;2.75

(define (make-from-mag-ang r theta)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos theta)))
	  ((eq? op 'imag-part) (* r (sin theta)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) theta)
	  (else
	   (error "Unkown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;2.78

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error " Bad tagged datum -- TYPE-TAG"))))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (cond ((equal? type-tag 'scheme-number) contents)
	(else
	 (cons type-tag contents))))

;;2.79

(define (equ? x y)
  (apply-generic 'equ? x y))

; Add into install-scheme-number package
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

; Add into install-rational-package
(define (equ-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))
(put 'equ? '(rational rational) equ-rat?)

; Add into install-complex-package
(define (equ-complex? x y)
  (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y))))
(put 'equ? '(complex complex) equ-complex?)

;;2.80

(define (=zero? x)
  (apply-generic '=zero? x))

; Add into install-scheme-number-package
(put '=zero? (lambda (x) (= x 0)))

; Add into install-rational-package
(define (=zero-rat? x)
  (= (numer x) 0))
(put '=zero? =zero-rat?)

; Add into install-complex-package
(define (=zero-complex? x)
  (and (= (real-part x) 0)
       (= (imag-part x) 0)))
(put 'zero? =zero-complex?)

;;2.81

;a)
由于对两个复数不存在指数运算，所以apply-generic会调用complex->complex然后再试着在表格中
找针对复数的指数运算。结果还是找不到，于是再接着调用complex->complex...就这样一直循环下去，
永远都不结束。

;b)
显然，Louis没有纠正同样类型参数的问题。apply-generic不能正确工作。

;c)
(define (apply-generic-2.81 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(if (= (length args) 2)
	  (let ((type1 (car type-tags))
		(type2 (cadr type-tags))
		(a1 (car args))
		(a2 (cadr args)))
	    (if (equal? type1 type2)
	      (error "No method for these types"
		     (list op type-tags))
	      (let ((t1->t2 (get-coercion type1 type2))
		    (t2->t1 (get-coercion type2 type1)))
		(cond (t1->t2
			(apply-generic-2.81 op (t1->t2 a1) a2))
		      (t2->t1
			(apply-generic-2.81 op a1 (t2->t1 a2)))
		      (else
			(error "No method for these types"
			       (list op type-tags)))))))
	  (error "No method for these types"
		 (list op type-tags)))))))

;;2.82
(define (apply-generic-2.82 op . args)
  (define (coerce-all args target-type)
    ;; coerce all the args to target-type if possible and return the coerced result;
    ;; otherwise return false.
    (define (coerce-iter arguments result)
      (if (null? arguments)
	result
	(let ((first-arg (car arguments)))
	  (cond ((equal? first-arg target-type)
		 (append result (list first-arg)))
		((get-coercion first-arg target-type)
		 (append result 
			 (list ((get-coercion first-arg target-type) first-arg))))
		(else #f)))))
    (coerce-iter args '()))
  (define (apply-generic-iter arguments)
    (cond ((null? arguments)
	   (error "No method for these types" 
	          (list op (map type-tag args))))
	  (else
	    (let ((type-tags (map type-tag args)))
	      (let ((proc (get op type-tags)))
		(if proc
		  (apply proc (map contents args)) 
		  (let ((coerced-args (coerce-all args (car arguments)))) 
		    (if coerced-args
		      (apply apply-generic-2.82
			     (append (list op)
				     coerced-args))
		      (apply-generic-iter (cdr arguments))))))))))
  (apply-generic-iter args))

;;2.83

; Add into install-scheme-number-package
(define (raise-number number)
  (if (equal? (type-tag number)
	      'scheme-number)
    (make-rat (contents number) 1)
    (error "Not a scheme-number -- RAISE-NUMBER" number)))

; Add into install-rational-package
(define (raise-rat rational)
  (if (equal? (type-tag rational)
	      'rational)
    (make-real
      (/ (numer rational)
	 (denom rational)))
    (error "Not a rational -- RAISE-RAT" rational)))

; Add into install-real-package
(define (raise-real real)
  (if (equal? (type-tag real)
	      'real)
    (make-complex-from-real-imag (contents real) 1)
    (error "Not a real number -- RAISE-REAL" real)))

(put 'raise 'real raise-real)
(put 'raise 'scheme-number raise-number)
(put 'raise 'rational raise-rational)

;;2.84

(define (apply-generic-2.84 op . args)
  (define (higher? type1 type2)
    (define (rank type)
      ((cond ((equal? type 'scheme-number) 1)
	     ((equal? type 'rational) 2)
	     ((equal? type 'complex) 3)
	     (else (error "No such type -- HIGHER?" type)))))
    (> (rank type1) (rank type2)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(if (= (length args) 2)
	  (let ((type1 (car type-tags))
		(type2 (cadr type-tags))
		(a1 (car args))
		(a2 (cadr args)))
	    (cond ((higher? type1 type2)
		   (apply-generic-2.84 op a1 (raise a2)))
		  ((higher? type2 type1)
		   (apply-generic-2.84 op (raise a1) a2))
		  (else
		    (error "No method for these types"
			   (list op type-tags))))))))))

;;2.85

; Add into install-complex-package
(define (project-complex complex)
  (make-real (real-part complex)))

; Add into install-real-package
(define (project-real real)
  (make-scheme-number (round (contents real))))

; Add into install-rational-package
(define (project-rational rational)
  (make-scheme-number (/ (numer rational) (denom rational))))

(put 'project 'complex project-complex)
(put 'project 'real project-real)
(put 'project 'rational project-rational)

(define (drop number)
  (let ((project-proc (get 'project (type-tag number))))
    (if project-proc 
      (let ((p (project number))) 
        (let ((r (raise p))) 
	  (if (equ? p r) 
	    (drop p) 
	    number)))
      number)))

(define (apply-generic-2.85 op . args)
  (define (higher? type1 type2)
    (define (rank type)
      ((cond ((equal? type 'scheme-number) 1)
	     ((equal? type 'rational) 2)
	     ((equal? type 'complex) 3)
	     (else (error "No such type -- HIGHER?" type)))))
    (> (rank type1) (rank type2)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(if (= (length args) 2)
	  (let ((type1 (car type-tags))
		(type2 (cadr type-tags))
		(a1 (car args))
		(a2 (cadr args)))
	    (cond ((higher? type1 type2)
		   (drop (apply-generic-2.84 op a1 (raise a2))))
		  ((higher? type2 type1)
		   (drop (apply-generic-2.84 op (raise a1) a2)))
		  (else
		    (error "No method for these types"
			   (list op type-tags))))))))))

;;2.91

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
	  (t2 (first-term L2)))
      (if (> (order t2) (order t1))
	(list (the-empty-termlist) L1)
	(let ((new-c (div (coeff t1) (coeff t2)))
	      (new-o (- (order t1) (order t2))))
	  (let (rest-of-result 
		 (div-terms (sub-terms L1 
			               (mul-terms (make-term new-o new-c) L2)) 
		            L2)) 
	    (list (cons (make-term new-o new-c) 
		        (car rest-of-result)) 
	          (cdr rest-of-result))))))))


