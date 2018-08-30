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


def venn(
    lefts: List[ElementType], rights: List[ElementType]
) -> Tuple[List[ElementType], List[ElementType], List[ElementType]]:
    """A venn diagram of the two sequences.

    A venn diagram shows the elements both sets contain individually as well as
    the elements they have in common.  We return a 3-tuple with the left only,
    common, and right only elements in its respective components.

    """

    return (
        [x for x in lefts if x not in rights],
        [x for x in lefts if x in rights],
        [x for x in rights if x not in lefts],
    )
