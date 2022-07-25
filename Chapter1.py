## The concept of iterative improvement in python

from typing import Callable


def iterative_improve(guess_good_enough: Callable, improve_guess: Callable) -> float:
    def go(guess: float) -> float:
        if guess_good_enough(guess):
            return guess
        else:
            return go(improve_guess(guess))

    return go(1.0)


# approximation to sqrt
def sqrt(x: float) -> float:
    def good_enough(guess: float, x: float) -> bool:
        return abs(guess**2 - x) < 0.001

    def improve_guess(guess: float, x: float) -> float:
        return (guess + x / guess) / 2

    return iterative_improve(
        guess_good_enough=lambda guess: good_enough(guess, x),
        improve_guess=lambda guess: improve_guess(guess, x),
    )


# sqrt(16) = 4.000000636692939

# approximation to the fixed-point of a function
def fixed_point(f: Callable) -> float:
    def close_enough(v1: float, v2: float) -> bool:
        return abs(v1 - v2) < 1e-4

    def improve_guess(f: Callable, guess: float) -> float:
        return f(guess)

    return iterative_improve(
        guess_good_enough=lambda guess: close_enough(f(guess), guess),
        improve_guess=lambda guess: improve_guess(f, guess),
    )


# this is the Golden ratio
golden_ratio = fixed_point(f=lambda x: 1 + 1 / x)  # 1.6179775280898876
