"""Tests for zfs.replicate.list."""

from typing import List, Set

from hypothesis import given
from hypothesis.strategies import integers, lists, sets

from zfs.replicate.list import *  # pylint: disable=unused-wildcard-import,wildcard-import


@given(lists(integers()))
def test_inits_length(elements: List[int]) -> None:
    """len(inits(elements)) == len(elements) + 1"""

    assert len(inits(elements)) == len(elements) + 1


@given(lists(integers(), min_size=2))
def test_inits_heads(elements: List[int]) -> None:
    """inits(elements)[:1] == [[], [elements[0]]"""

    assert inits(elements)[0] == []
    assert inits(elements)[1] == [elements[0]]


@given(lists(integers()))
def test_inits_monotonic_length(elements: List[int]) -> None:
    """[len(x) for xs in inits(elements)] == range(len(elements) + 1)"""

    lengths = [len(xs) for xs in inits(elements)]

    assert lengths == list(range(len(elements) + 1))


@given(sets(integers()), sets(integers()))
def test_venn_subsets(lefts: Set[int], rights: Set[int]) -> None:
    """all combinations of venn with subsets"""

    r_lefts: List[int]
    r_middles: List[int]
    r_rights: List[int]

    r_lefts, r_middles, r_rights = venn(list(lefts), list(lefts | rights))

    assert (set(r_lefts), set(r_middles), set(r_rights)) == (set(), lefts, rights - lefts)

    r_lefts, r_middles, r_rights = venn(list(lefts | rights), list(rights))

    assert (set(r_lefts), set(r_middles), set(r_rights)) == (lefts - rights, rights, set())


@given(lists(integers()))
def test_venn_disjoint(both: List[int]) -> None:
    """venn with disjoint"""

    e_lefts = list(filter(lambda x: x % 2 == 0, both))
    e_rights = list(filter(lambda x: x % 2 != 0, both))

    r_lefts: List[int]
    r_middles: List[int]
    r_rights: List[int]

    r_lefts, r_middles, r_rights = venn(list(e_lefts), list(e_rights))

    assert (list(r_lefts), list(r_middles), list(r_rights)) == (e_lefts, [], e_rights)
