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


;; Euclid's Algorithm for calculating the greatest common divisor -----

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

;; Primality -----
; Find the smallest divisor of an integer n.
; We first test whether we can divide by 2, and if not, we test if we can divide by 3 and so on.
; However, we can stop the search early, if we do not find a divisor smaller than sqrt(n), because
; if a number n is not prime, it has a divisor smaller or eqal to sqrt(n).
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((?divides test-divisor n) test-divisor)
        (else (find-divisor n (next-test-divisor test-divisor)))))
(define (?divides a b)
  (= (remainder b a) 0))

; instead of increasing the test-divisor by one, it is sufficient to test odd numbers after division by 2 has failed!
(define (next-test-divisor n)
  (if (= n 2)
      3
      (+ n 2)))
  
; a number is prime, if the smallest-divisor is n itsself:
(define (prime? n)
  (= (smallest-divisor n) n))

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

;; Fermat Test for primality
; check that a^n % n = a for every random number (integer) < n 
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; Fast test for primality using Fermat method
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Some numbers fool the Fermat-test: the Carmichael numbers! for these numbers the condition
; a^n % n = a for every random integer < n holds true although they are not prime numbers!
; e.g. 561 (divisible by 3) , 1105 (divisible by 5) ...


;; Timed Search for Primes -----
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

(define (search-for-primes min max)
  (if (= min max)
      '()
      (begin
        (timed-prime-test min)
        (search-for-primes (+ min 1) max))))

; 1009, 1013 and 1019 are the first 3 primes > 1000, runtime about 4 (3 with next-test-divisor function)
; 10007, 10009, 10037 are the first 3 primes > 10k, runtime about 6 (4 with next-test-divisor function)
; 100003, 100019, 100043 are the frist 3 primes > 100k, runtime about 17 (12 with next-test-divisor function)

;; Higher-order procedures -----
; Summation (Sigma)
; Sum over "term" (= a procedure) over the values a to b, where "next" is a procedure that determines how to set the 
; next value between a and b 
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; use the sum procedure to calculate the sum of cubes
(define (sum-cubes a b)
  (sum cube a inc b))

; use the sum procedure to calculate sum of numbers from a to b (using an identity term)
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

; Approximation to pi
; this sum converges to pi/8 very slowly
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; (* 8 (pi-sum 1 1000))

; use the sum procedure to approximate the integral of a function
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

; (integral cube 0 1 0.01) gives 0.24998750000000042, true value would be 1/4

;; Simpson's rule for approximation of integrals -----

(define (simpson f a b n)
  ; step size in integration h
  (let ((h (/ (- b a) n)))
    ; weight factor in addition of terms
    (define (simpson-weight i n)
      (cond ((= i 0) 1)
            ((= i n) 1)
            ((even? i) 2)
            (else 4)))
    ; step-wise addition, recursive
    (define (simpson-go f a counter)
      (if (> counter n)
          0
          (+ (* (simpson-weight counter n) (f a))
             (simpson-go f (+ a h) (inc counter)))))
    ; return the sum scaled by h/3 
    (* (/ h 3.0)
       (simpson-go f a 0))))

; Iterative version of the sum procedure -----
(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; (sum2 square 0 inc 3) equals 14

;; Product procedures -----
; Recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
; Iterative
(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; definition of factorial using product
(define (factorial-p n)
  (product2 identity 1 inc n))

; approximation to pi using product aka Wallis product
(define (pi-approx n)
  (define (term x)
    (+ 1
       (/ 1
          (- (* 4 (square x))
             1))))
  (* 2.0 (product2 term 1 inc n)))


; (pi-approx 1000)

;; Accumulate: for defined combinations of terms (product, sum etc.), recursive -----
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum3 term a next b)
  (accumulate + 0 term a next b))

(define (product3 term a next b)
  (accumulate * 1 term a next b))

;;  Accumulate: for defined combinations of terms (product, sum etc.), iterative -----
(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Filtered accumulate (only combine terms if the values between a and b fulfill a certain condition)
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

; sum of squares of prime numbers in interval a to b
(define (sum-of-squares-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

; product of all positive integers <n that are relatively prime to n
(define (product-rel-prime n)
  (define (rel-prime? i)
    (=(gcd i n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) rel-prime?))

;; LET is syntactic sugar for an expression involving a lambda function -----
; ft = xa^2 + yb + ab with a = 1 + xy and b = 1 - y
(define (ft x y)
  ((lambda (a b) (+ (* x (square a))
                    (* y b)
                    (* a b))) (+ 1 (* x y)) (- y 1)))

(define (ft-twin x y)
  (let ((a (+ 1 (* x y)))
        (b (- y 1)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; > #t
;(= (ft 1 1) (ft-twin 1 1))

;; Roots of equations with the half-interval method -----
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))


(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign, so there is no root in this interval!")))))

;(half-interval-method sin 2.0 4.0) evaluates to 3.14111328125

;; Fixed Points -----
(define tolerance 0.00001)
(define (fix-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; the golden ratio is a fix point of function f(x) = 1 + 1/x
; (fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0) evaluates to about 1.618

; (fix-point (lambda (x) (/ (log 1000) (log x))) 2) takes 34 iterations
; (fix-point (lambda (x) (average x  (/ (log 1000) (log x)))) 2) takes 9 iterations because
; of the use of average damping !! (instead of finding the fix point of f(x), find the fixpoint
; of the average: (f(x) + x)/2 