#lang sicp
;; pascals triangle aka Binomial Coefficients
(define (pascal n k)
  (cond ((= k 0) 1)
        ((= k n) 1)
        (else (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k)))))



; approximation to calculation of sin
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine-approx angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine-approx (/ angle 3.0)))))

; recursive process for calculating exponentiation 
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


; iterative process for calculating exponentiation
(define (expt2 b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))


; fast exponentiation
(define (square x)
  (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


; fast exponentiation, iterative

(define (fast-expt2 x n)
  (fast-expt2-iter x n 1))

(define (fast-expt2-iter x counter a)
  (cond ((= counter 0) a)
        ((even? counter) (fast-expt2-iter (square x)
                                          (/ counter 2)
                                          a))
        (else (fast-expt2-iter (square x)
                               (/ (- counter 1) 2)
                               (* a x)))))
      
(fast-expt2 2 2)
