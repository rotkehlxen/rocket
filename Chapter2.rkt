#lang sicp
;; Rational number arithmetic -----

; we need the greatest common divisor to reduce the numerator/denominator to lowest terms
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; constructor for a rational number:
; reduce to lowest terms and if the rational number is negative, keep the minus sign in the numerator
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (> (/ n d) 0)
        (cons (abs (/ n g)) (abs (/ d g)))
        (cons (- (abs (/ n g))) (abs (/ d g)))))) ; cons = construct, a pair of two values

; numerator
(define (numer x) ; car = contents of address part of register
  (car x))

; denominator
(define (denom x)
  (cdr x)) ; cdr = contents of decrement part of register  

; Addition, Subtraction, Multiplication, Division and Equality of two rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Printed representation of a rational number
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

; (print-rat (add-rat one-half one-third)) ; 5/6
; (print-rat (mul-rat one-half one-third)) ; 1/6


;; Points and Line segments in a plane -----

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (mid-point segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2.0)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2.0)))

; (print-point (mid-point (make-segment (make-point 1 2) (make-point 6 4))))  ; (3.5,3.0)

; Rectangle, constructed from 3 points -----
; (assuming that the two segments formed by connecting these 3 points enclose a right angle)
; ll: lower left
; lr: lower right
; ur: upper right corner

(define (make-rect ll lr ur)
  (cons (cons ll lr) ur))

(define (rect-ll rectangle)
  (car (car rectangle)))

(define (rect-lr rectangle)
  (cdr (car rectangle)))

(define (rect-ur rectangle)
  (cdr rectangle))

; (print-point (rect-lr (make-rect (make-point 1 1)
;                       (make-point 2 1)
;                       (make-point 2 3))))  ; (2,1)
(define (square x)
  (* x x))

(define (rect-faces r)
  (let ((llx (x-point (rect-ll r)))
        (lly (y-point (rect-ll r)))
        (lrx (x-point (rect-lr r)))
        (lry (y-point (rect-lr r)))
        (urx (x-point (rect-ur r)))
        (ury (y-point (rect-ur r))))
    (cons (sqrt (+ (square (- lrx llx))
                   (square (- lry lly))))
          (sqrt (+ (square (- urx lrx))
                   (square (- ury lry)))))))

(define r (make-rect (make-point 1 1) (make-point 2 1) (make-point 2 3)))
; (rect-faces r)  ; returns (1 . 2)
; if we have the abstraction layer "faces", it does not matter how the square is implemented, we can always derive
; perimeter and area from the faces

(define (perimeter r)
  (let* ((faces (rect-faces r))
         (a (car faces))
         (b (cdr faces)))
    (* (+ a b) 2)))

(define (area r)
  (let* ((faces (rect-faces r))
         (a (car faces))
         (b (cdr faces)))
    (* a b)))

; (area r) ; 2
; (perimeter r) ; 6

;; Implementation of cons, car and crd in wich the data are expressed as procedures -----

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

;; Implementation of cons, by representing it as the product 2^a * 3^b -----
(define (cons3 x y)
  (* (expt 2 x)
     (expt 3 y)))

; check how often we can evenly divide by 2 to find a
(define (car3 pair)
  (define (go counter p)
    (if (not (= (remainder p 2) 0))
        counter
        (go (inc counter) (/ p 2))))
  (go 0 pair))

; check how often we can evenly divide by 3 to find b
(define (cdr3 pair)
  (define (go counter p)
    (if (not (= (remainder p 3) 0))
        counter
        (go (inc counter) (/ p 3))))
  (go 0 pair))

; (cdr3 (cons3 0 10)) ; 10

;; Church numerals -----
; 0 : apply f to x zero times (return x itself)
; 1 : apply f to z once
; 2 : apply f to z twice ...

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; ((zero square) 3)                    ; 3
; (((add-1 zero) square) 3)            ; 3^2
; (((add-1 (add-1 zero)) square) 3)    ; (3^2)^2

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; ((one square) 3)  ; 3^2
; ((two square) 3)  ; (3^2)^2

(define (add-church a b)
  (lambda (f)
    (lambda (x) ((b f) ((a f) x)))))

; (((add-church one two) square) 3)  ; ((3^2)^2)^2 = 6561
; (((add-church two one) square) 3)  ; ((3^2)^2)^2 = 6561

;; Interval arithmetic -----
; You can measure values with a certain precision only, e.g. 6.8 Ohms with 10 % tolerance,
; so your measurement has a lower-bound of 6.8 - 0.68 = 6.12 and an upper-bound of 6.8 + 0.68 = 7.48
; this affects the precision of the sum/product of measured properties, as implemented here

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

; meaning of division not clear when y spans zero, also, when the upper or lower bound
; is exactly zero, we get a "division by zero" error

(define (div-interval-v2 x y)
  (let ((l (lower-bound y))
        (u (upper-bound y)))
    (if (and (<= l 0) (>= u 0))
        (error "Cannot perform division because interval y spans zero")
        (make-interval (/ 1.0 u)
                       (/ 1.0 l)))))

; (define x (make-interval 1 2))
; (define y (make-interval -2 1))
; (div-interval-v2 x y)

; alternvative implementation for interval multiplication
(define (pos-interval? x)
  (and (>= (upper-bound x) 0) (>= (lower-bound x) 0)))
(define (neg-interval? x)
  (and (< (upper-bound x) 0) (<= (lower-bound x) 0)))
(define (mixed-interval? x)
  (and (< (lower-bound x) 0) (> (upper-bound x) 0)))

(define (mul-interval-v2 x y)
  (let ((lx (lower-bound x))
        (ly (lower-bound y))
        (ux (upper-bound x))
        (uy (upper-bound y)))
    (cond ((and (pos-interval? x) (pos-interval? y)) (make-interval (* lx ly)
                                                                    (* ux uy)))
          ((and (neg-interval? x) (neg-interval? y)) (make-interval (* ux uy)
                                                                    (* lx ly)))
          ((and (pos-interval? x) (mixed-interval? y)) (make-interval (* ux ly)
                                                                             (* ux uy)))
          ((and (mixed-interval? x) (pos-interval? y)) (make-interval (* lx uy)
                                                                              (* ux uy)))
          ((and (neg-interval? x) (mixed-interval? y)) (make-interval (* lx uy)
                                                                              (* lx ly)))
          ((and (mixed-interval? x) (neg-interval? y)) (make-interval (* ux ly)
                                                                             (* lx ly)))
          ((and (pos-interval? x) (neg-interval? y)) (make-interval (* ux ly)
                                                                    (* lx uy)))
          ((and (neg-interval? x) (pos-interval? y)) (make-interval (* lx uy)
                                                                    (* ux ly)))
          (else (make-interval (min (* lx uy)
                                    (* ux ly))
                               (max (* lx ly)
                                    (* ux uy)))))))

; (define x (make-interval 1 2))
; (define y (make-interval -1 3))
; (mul-interval-v2 x y)

; Represent an interval as center and percent uncertainty/tolerance
(define (make-center-percent c perc)
  (let ((width (/ (* c perc) 100)))
    (make-interval (- c width) (+ c width))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (perc x)
  (let ((width (/ (- (upper-bound x)
                     (lower-bound x))
                  2)))
    (* (/ width (center x)) 100)))
  
; (perc (make-center-percent 20 5))   ; 5
; (center (make-center-percent 20 5)) ; 20

; Assuming that the tolerances of intervals are small, the tolerance of the product of two intervals
; is approximately the sum of the tolerances of the two intervals
; e.g. ux = cx(1+tolx) and uy = (cy(1+toly), then the upper bound of x*y is
; cx (1+tolx) cy (1+toly) =
; cx cy (1 + tolx + toly + tolx * toly)
; ~ cx cy (1 + [tolx + toly])
; because tolx * toly >> 0 if tolx and toly are small

; Parallel resistors -----
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)

                                (div-interval one r2)))))

; par1 and par2 are equivalent expressions, but return different results 

; (define r1 (make-interval 1 1.1))
; (define r2 (make-interval 2 2.1))

; (par1 r1 r2)
; (par2 r1 r2)

; (define r1 (make-interval 1 1))
; (define r2 (make-interval 2 2.5))

; Most notworthy: a/a = 1 is not true for intervals, because we do not know whether measured values
; with the same interval are actually exactly the same values

; (div-interval r2 r2) ; (0.8, 1.25)

; Lists -----
; obtain the nth item in the list (starting to count at zero)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (dec n))))

; (list-ref (list 1 2 3 4) 3)  ; 4
; recursive definiton for length of list
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; iterative implementation for length of a list
(define (length-v2 items)
  (define (go x counter)
    (if (null? x)
        counter
        (go (cdr x) (inc counter))))
  (go items 0))

; (length-v2 (list 1 2 3))

; append one list to another

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; (append (list 1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)

(define (last-pair x)
  (define (go last x)
    (if (null? x)
        last
        (go (car x) (cdr x))))
  (go nil x))

; (last-pair (list 1 2 100))  ; 100
; (last-pair (list))          ; ()

; reverse list
(define (reverse x)
  (define (go reversed-x rest)
    (if (null? rest)
        reversed-x
        (go (cons (car rest) reversed-x) (cdr rest))))
    (go nil x))

; (reverse (list 3 4 5 8 10)) ; (10 8 5 4 3)

;; Counting change from Chapter 1, now using lists -----
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-values)
  (null? coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (first-denomination coin-values)
  (car coin-values))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

; (cc 100 us-coins) ; 292

; Dot notation for functions with arbitrary number of arguments -----

(define (only-even x)
  (if (null? x)
      nil
      (if (even? (car x))
          (cons (car x) (only-even (cdr x)))
          (only-even (cdr x)))))


(define (only-uneven x)
  (if (null? x)
      nil
      (if (not (even? (car x)))
          (cons (car x) (only-uneven (cdr x)))
          (only-uneven (cdr x)))))

(define (same-parity x . y)
  (if (even? x)
      (cons x (only-even y))
      (cons x (only-uneven y))))

; (same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
; (same-parity 2 3 4 5 6 7)   ; (2 4 6)

;; Scale every element in a list by a factor -----

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* factor (car items)) (scale-list (cdr items) factor))))

; (scale-list (list 1 2 3 4) 0.5)
; generalise this for any kind of procedure (with one arguemnt)

(define (map2 proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map2 proc (cdr items)))))

; (map2 abs (list 1 -2 3 -4)) ; (1 2 3 4)
; epxress the list scaling in terms of map2 function
(define (scale-list2 items factor)
  (map2 (lambda (x) (* factor x)) items))

; squaring a list, without map2 and with map2

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map2 square items))

; an implementation of for-each (the cond expression allows one more line of code to be specified within a block
(define (my-for-each proc items)
  (cond ((not (null? items))
      (proc (car items))
      (my-for-each proc (cdr items)))))

; (my-for-each (lambda (x) (newline) (display x)) (list 1 2 3))

;; Count leaves of the tree structure representation of a nested list
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; (count-leaves (list 1 2 (list 1 2 3)))

;; access element 7 from different heavily nested lists
; (car (cdaddr (list 1 3 (list 5 7) 9)))                                              ; 7
; (caar (list (list 7)))                                                              ; 7
; (cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))  ; 7


; Cons combined with lists
; (define x (list 1 2 3))
; (define y (list 4 5 6))

; (append x y) ; (1 2 3 4 5 6)
; (cons x y)   ; ((1 2 3) 4 5 6)
; (list x y)   ; ((1 2 3) (4 5 6))

; Deep reversing of lists -----
; (deep-reverse ((1 2) (3 4))) should be ((4 3) (2 1))
; I reasonded that we can modify the reverse function as follows:
; 1) call deep-reverse on every element added to the reversed list
; 2) introduce a bottom case for reversing integers (or better: not pairs!), which
;    is to return the integer itsself

(define (deep-reverse x)
  (define (go reversed-x rest)
    (cond ((null? rest) reversed-x)
          ((not (pair? rest)) rest)
          (else (go (cons (deep-reverse (car rest)) reversed-x) (cdr rest)))))
  (go nil x))


; (deep-reverse (list 1 (list 2 3)))            ; ((3 2) 1)
; (deep-reverse (list (list 1 2) (list 3 4)))   ; ((4 3) (2 1))
; (deep-reverse 1)                              ; 1
; (deep-reverse (list 1 2))                     ; (2 1)
; (deep-reverse (list 1 (list 2 (list 3 4))))   ; (((4 3) 2) 1)

; Fringe (or flatten a list)
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))


; (fringe (list 1 2 (list 3 4 (list 5 6 7)))) ; (1 2 3 4 5 6 7)

; Mobile -----

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

; the weight of a branch
(define (total-weight-branch branch)
  (let ((struc (branch-structure branch)))
    (if (not (pair? struc))
        struc
        (+ (total-weight-branch (left-branch struc))
           (total-weight-branch (right-branch struc))))))

; the weight of a mobile expressed as the weight of its two branches
(define (total-weight mobile)
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

(define mymobile (make-mobile (make-branch 2 (make-mobile (make-branch 2 2) (make-branch 2 2)))
                              (make-branch 2 4)))

; (total-weight mymobile) ; is 2 + 2 + 4 = 8

; Mobile balanced?

(define (torque-branch b)
  (let ((l (branch-length b))
        (s (branch-structure b)))
    (if (not (pair? s))
        (* l s)
        (* l (total-weight s)))))

(define (branch-balanced? b)
  (let ((s (branch-structure b)))
    (if (not (pair? s))
        #t
        (mobile-balanced? s))))

(define (mobile-balanced? m)
  (let* ((lb ( left-branch m))
         (rb (right-branch m))
         (tt (= (torque-branch lb) (torque-branch rb))))
    (and tt (branch-balanced? lb) (branch-balanced? rb))))

; (mobile-balanced? mymobile) ; #t

;; Mapping over Trees -----
; A nested list can be seen as a tree. We can map a function to every element in a tree,
; e.g. scale every element by a factor

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

; (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10) ; (10 (20 (30 40) 50) (60 70))
; or square every element:

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; alternatively we can use map to solve this problem:

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor))) tree))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree))) tree))

; and for any functions in general:
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree))) tree))

(define (square-tree3 tree)
  (tree-map square tree))


;; all subsets of set (set represented as list with distinct elements) -----
; The idea is that all combinations can be represented as the sum of
; 1) all combinations that don't use the first element in the set
; 2) combinations of the first element in the set with all combinations that don't use the first element

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (rest) (cons (car s) rest))
                     rest)))))

; (subsets (list 1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
