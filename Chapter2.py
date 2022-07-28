from typing import Callable


""" 
Implementation of the lisp data structure for a pair of data, called cons, and its selectors 
car (first element) and cdr (the second element). This example shows how data can be replaced
by procedures.
"""


def cons(x: float, y: float) -> Callable:
    return lambda f: f(x, y)


def car(_cons: Callable) -> float:
    return _cons(lambda p, q: p)


def cdr(_cons: Callable) -> float:
    return _cons(lambda p, q: q)
