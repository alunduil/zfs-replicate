"""Tests for zfs.replicate.list."""
from typing import List, Set

from hypothesis import given
from hypothesis.strategies import integers, lists, sets

from zfs.replicate import list as sut


@given(lists(integers()))  # type: ignore[misc]
def test_inits_length(elements: List[int]) -> None:
    """len(inits(elements)) == len(elements) + 1."""
    assert len(sut.inits(elements)) == len(elements) + 1  # nosec


@given(lists(integers(), min_size=2))  # type: ignore[misc]
def test_inits_heads(elements: List[int]) -> None:
    """inits(elements)[:1] == [[], [elements[0]]."""
    assert sut.inits(elements)[0] == []  # nosec
    assert sut.inits(elements)[1] == [elements[0]]  # nosec


@given(lists(integers()))  # type: ignore[misc]
def test_inits_monotonic_length(elements: List[int]) -> None:
    """[len(x) for xs in inits(elements)] == range(len(elements) + 1)."""
    lengths = [len(xs) for xs in sut.inits(elements)]

    assert lengths == list(range(len(elements) + 1))  # nosec


@given(sets(integers()), sets(integers()))  # type: ignore[misc]
def test_venn_subsets(lefts: Set[int], rights: Set[int]) -> None:
    """All combinations of venn with subsets."""
    r_lefts: List[int]
    r_middles: List[int]
    r_rights: List[int]

    r_lefts, r_middles, r_rights = sut.venn(list(lefts), list(lefts | rights))

    assert (set(r_lefts), set(r_middles), set(r_rights)) == (  # nosec
        set(),
        lefts,
        rights - lefts,
    )

    r_lefts, r_middles, r_rights = sut.venn(list(lefts | rights), list(rights))

    assert (set(r_lefts), set(r_middles), set(r_rights)) == (  # nosec
        lefts - rights,
        rights,
        set(),
    )


@given(lists(integers()))  # type: ignore[misc]
def test_venn_disjoint(both: List[int]) -> None:
    """Venn with disjoint."""
    e_lefts = list(filter(lambda x: x % 2 == 0, both))
    e_rights = list(filter(lambda x: x % 2 != 0, both))

    r_lefts: List[int]
    r_middles: List[int]
    r_rights: List[int]

    r_lefts, r_middles, r_rights = sut.venn(list(e_lefts), list(e_rights))

    assert (list(r_lefts), list(r_middles), list(r_rights)) == (  # nosec
        e_lefts,
        [],
        e_rights,
    )  # nosec
