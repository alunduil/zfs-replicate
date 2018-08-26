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

    assert lengths == range(len(elements) + 1)


@given(lists(integers()), lists(integers))
def test_venn_subsets(lefts, rights):
    """all combinations of venn with subsets"""

    assert venn(lefts, lefts + rights) == ([], lefts, rights)
    assert venn(lefts + rights, rights) == (lefts, rights, [])


@given(lists(integers()))
def test_venn_disjoint(both):
    """venn with disjoint"""

    lefts = filter(lambda x: x % 2 == 0, both)
    rights = filter(lambda x: x % 2 != 0, both)

    assert venn(lefts, rights) == (lefts, [], rights)
