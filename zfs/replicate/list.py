"""Common List Functions."""

from typing import List, Sequence, Tuple, TypeVar

ElementType = TypeVar("ElementType")


def inits(elements: Sequence[ElementType]) -> Sequence[Sequence[ElementType]]:
    """All initial segments of the given list.

    The shortest lists are first.

    >>> inits(["a", "b", "c"])
    [[], ['a'], ['a', 'b'], ['a', 'b', 'c']]

    """

    return [elements[:n] for n in range(len(elements) + 1)]
