"""Common List Functions."""

from typing import List, TypeVar

A = TypeVar("A")

def inits(elements: List[A]) -> List[List[A]]:
    """All initial segments of the given list.

    The shortest lists are first.

    >>> inits(["a", "b", "c"])
    [[], ['a'], ['a', 'b'], ['a', 'b', 'c']]

    """

    return [elements[:n] for n in range(len(elements) + 1)]
