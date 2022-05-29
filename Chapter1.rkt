#lang sicp
;; Factorial -----
; The following implementations are RECURSIVE PROCEDURES,
; however the resulting process is recursive in factorial, 
; but iterative in factorial-v2


; factorial: linear recursion
(define (factorial n) 
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; factorial: linear iteration
(define (factorial-v2 n)
  (fact-iter 1 1 n))
  
(define (fact-iter counter product max-count)
   (if (> counter max-count)
       product
       (fact-iter (+ counter 1) 
                  (* counter product)
                  max-count)))

; in languages without tail-call-optimisation (TCO) factorial-v2 would STILL be executed resursively
; (and thus requires a stack and could potentially lead to stackoverflow) - thats why these languages
; require looping constructs to perform iterations (for, while etc.)

; every iteration can be stated as a recursive procedure!! 


;; Newtons method for calculating square roots -----
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- x (* guess guess))) 
     0.001))

(define (good-enough2? guess x)
  (< (abs (/ (- guess (improve guess x))
             guess))
     1e-12))


(define (sqrt-iter guess x)
  (if (good-enough2? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

;; Implementation of addition -----
;; assuming that the computer only knows how to increment or decrement by the value of 1
;; addition can be seen as moving apples from one plate (a) to the other (b) one by one


; iterative process
(define (plus-iter a b)
  (if (= a 0) 
    b
    (plus-iter (dec a) (inc b))))

; recursive process
(define (plus-rec a b)
  (if (= a 0)
      b
      (inc (plus-rec (dec a) b))))

; of course both implementations are super slow, but the recursive process has the additional "problem" of
; potential stack overlfow

;; Ackermann function 
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; All ways (combinations) of making change for a defined amount of money when having coins 1, 5, 10, 25 and 50 -----
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 5) 50)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 1) 1)))

(define (change-options amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= kinds-of-coins 0) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

;; Fibonacci numbers -----

; recursive
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                  (fibonacci (- n 2))))))

; iterative

(define (fibo n)
  (fibo-iter 1 0 n))

(define (fibo-iter last before-last counter)
  (if (= counter 0)
    before-last
    (fibo-iter (+ last before-last) last (- counter 1))))


; fibo related function, recursive

(define (f n)
  (if (< n 3)
    n
    (+ (* 1 (f (- n 1)))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

; fibo related function, iterative

(define (fi n)
  (fi-iter 2 1 0 n))

(define (fi-iter last before-last bbefore-last counter)
  (if (= counter 0)
    bbefore-last
    (fi-iter (+ (* 1 last)
                (* 2 before-last)
                (* 3 bbefore-last))
             last
             before-last
             (- counter 1))))


;; Pascals triangle aka Binomial Coefficients
(define (pascal n k)
  (cond ((= k 0) 1)
        ((= k n) 1)
        (else (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k)))))


;; Approximation to calculation of Sin
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine-approx angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine-approx (/ angle 3.0)))))

;; Exponentiation -----
;; Recursive process for calculating exponents 
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; iterative process for calculating exponentiation
(define (expt2 b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))


;; Fast exponentiation, recursive
(define (square x)
  (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;; Fast exponentiation, iterative

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
      
