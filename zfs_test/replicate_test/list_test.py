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
