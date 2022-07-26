#lang sicp
;; Rational number arithmetic -----

; we need the greatest common divisor to reduce the numerator/denominator to lowest terms
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; constructor for a rational number
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (> (/ n d) 0)
        (cons (abs (/ n g)) (abs (/ d g)))
        (cons (- (abs (/ n g))) (abs (/ d g))))))


  ; cons = construct, a pair of two values

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


