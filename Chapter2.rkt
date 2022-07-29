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

; (define x (make-interval 1 2))
; (define y (make-interval 3 4))
; (sub-interval x y)
