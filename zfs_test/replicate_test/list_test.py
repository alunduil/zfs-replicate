"""Tests for zfs.replicate.list."""

from hypothesis import given
from hypothesis.strategies import integers, lists

from zfs.replicate.list import *  # pylint: disable=unused-wildcard-import,wildcard-import


@given(lists(integers()))
def test_inits_length(elements):
    """len(inits(elements)) == len(elements) + 1"""

    assert len(inits(elements)) == len(elements) + 1


@given(lists(integers(), min_size=2))
def test_inits_heads(elements):
    """inits(elements)[:1] == [[], [elements[0]]"""

    assert inits(elements)[0] == []
    assert inits(elements)[1] == [elements[0]]


@given(lists(integers()))
def test_inits_monotonic_length(elements):
    """[len(x) for xs in inits(elements)] == range(len(elements) + 1)"""

    lengths = [len(xs) for xs in inits(elements)]

    assert lengths == list(range(len(elements) + 1))


@given(lists(integers()), lists(integers()))
def test_venn_subsets(lefts, rights):
    """all combinations of venn with subsets"""

    r_lefts, r_middles, r_rights = venn(lefts, lefts + rights)

    assert (list(r_lefts), list(r_middles), list(r_rights)) == ([], lefts, rights)

    r_lefts, r_middles, r_rights = venn(lefts + rights, rights)

    assert (list(r_lefts), list(r_middles), list(r_rights)) == (lefts, rights, [])


@given(lists(integers()))
def test_venn_disjoint(both):
    """venn with disjoint"""

    e_lefts = list(filter(lambda x: x % 2 == 0, both))
    e_rights = list(filter(lambda x: x % 2 != 0, both))

    r_lefts, r_middles, r_rights = venn(e_lefts, e_rights)

    assert (list(r_lefts), list(r_middles), list(r_rights)) == (e_lefts, [], e_rights)
