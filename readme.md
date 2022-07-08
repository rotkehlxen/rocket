# Lessons learned from "Structure and Interpretation of Computer Programs"
## LET
The `let` expression allows for the definition of local variables, however
it is only syntactic sugar for an expression that involves an anonymous `lambda` function.
For example, if you define a function `ft = xa^2 + yb + ab` with `a = 1 + xy` and `b = 1 - y`, 
you could write (version 1):

```lisp
(define (ft x y)
  ((lambda (a b) (+ (* x (square a))
                    (* y b)
                    (* a b))) (+ 1 (* x y)) (- y 1)))
```
In this expression we create a lambda function and immediately apply it to
its arguments a and b.
The following expression is a 100% equivalent for the lisp interpreter
(version 2):

```lisp
(define (ft x y)
  (let ((a (+ 1 (* x y)))
        (b (- y 1)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
```
There is an important consequence of this equivalence: When you define
variables in a `let` expression, these can not be built depending on
another, like e.g. a = 1 and b = a + 1, because these two variables are
nothing more than the arguments in a lambda function (compare with version 1).

