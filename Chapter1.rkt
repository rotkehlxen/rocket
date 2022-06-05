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

;; Integer multiplication -----
;; Fast integer multiplication, recursive
(define (double x)
  (* 2 x))
(define (halve x)
  (/ x 2))

(define (fast-mul n x)
  (cond ((= n 0) 0)
        ((even? n) (double (fast-mul (halve n) x)))
        (else (+ x (fast-mul (- n 1) x)))))

;; Fast integer multiplication, iterative
(define (fast-mul-v2 n x)
  (fast-mul-iter n 0 x))

(define (fast-mul-iter n y z)
  (cond ((= n 0) y)
        ((even? n) (fast-mul-iter (halve n) y (double z)))
        (else (fast-mul-iter (- n 1) (+ y z) z))))


;; Fast Fibonacci numbers calculation -----
; the state transformation for Fibonacci number calculation is a generalisation of the following transformation
; a <- bq + aq + ap
; b <- bp + aq
; where p=0 and q=1.
; Applying the transformation twice (aka squaring) yields a transformation of the same shape with
; p'=p^2 + q^2 and
; q'=q^2 + 2pq
; which can be used for constructing a fast iterative algorithm: 

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 q p))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; Euclid's Algorithm for calculating the greatest common divisor

; 1) a and b have a greatest common divisor g, that means a and b are multiples of g
; 2) if we subtract b from a, that means we subtract a multiple of g, which means the resulting number will also be
; a multiple of g
; 3) we can subtract b as many times from a as we want, we always obtain a number which is a multiple of g
; 4) we can divide a by b and the remainder will be a multiple of g (because that means subtracting b as often as
; possible)
; 5) we can also derive the greatest common divisor from the remainder of a/b and b :-) 
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Primality
; Find the smallest divisor of an integer n.
; We first test whether we can divide by 2, and if not, we test if we can divide by 3 and so on.
; However, we can stop the search early, if we do not find a divisor smaller than sqrt(n), because
; if a number n is not prime, it has a divisor smaller or eqal to sqrt(n).
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((?divides test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (?divides a b)
  (= (remainder b a) 0))

;; Fast x^n % m calculation
; if n is even, we can still use successive squaring to speed up exponentiation because of the following relation:
;   x^n % m
; = (expmod x n m)
; = (x^(n/2) * x^(n/2)) % m
; = [(x^(n/2) % m) * (x^(n/2) % m)] % m
; = (x^(n/2) % m)^2 % m
; = (expmod x n/2 m)^2 % m

(define (expmod x n m)
  (cond ((= n 0) 1)
        ((even? n) (remainder (square (expmod x (/ n 2) m)) m))
        (else (remainder (* x (expmod x (- n 1) m)) m))))

