"""Common List Functions."""

from collections.abc import Sequence
from typing import Generic, NamedTuple, TypeVar

__all__ = ("Venn", "inits", "venn")

T = TypeVar("T")


def inits(elements: Sequence[T]) -> Sequence[Sequence[T]]:
    """All initial segments of the given list.

    The shortest lists are first.

    >>> inits(["a", "b", "c"])
    [[], ['a'], ['a', 'b'], ['a', 'b', 'c']]
    """
    return [elements[:n] for n in range(len(elements) + 1)]


class Venn(NamedTuple, Generic[T]):
    """The three groups of a venn diagram of two lists."""

    left_only: list[T]
    """Elements of the left list that the right list lacks."""

    common: list[T]
    """Elements that both lists contain, in the left list's order."""

    right_only: list[T]
    """Elements of the right list that the left list lacks."""


def venn(
    lefts: list[T],
    rights: list[T],
) -> Venn[T]:
    """Calculate venn diagram of the two sequences.

    A venn diagram shows the elements both sets contain individually as
    well as the elements they have in common.
    """
    return Venn(
        left_only=[x for x in lefts if x not in rights],
        common=[x for x in lefts if x in rights],
        right_only=[x for x in rights if x not in lefts],
    )
