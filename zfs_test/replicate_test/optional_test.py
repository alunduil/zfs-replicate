"""Tests for zfs.replicate.optional."""

import pytest
from hypothesis import given
from hypothesis.strategies import integers, lists, none, one_of

from zfs.replicate import optional


class TestValue:
    """A present value comes back, and a missing one raises."""

    def test_none(self) -> None:
        """optional.value(None) → RuntimeError."""
        with pytest.raises(RuntimeError):
            optional.value(None)

    @given(integers())
    def test_not_none(self, value: int) -> None:
        """optional.value(value) == value."""
        assert optional.value(value) == value


class TestValues:
    """Nothing missing survives, and everything present keeps its order."""

    @given(lists(one_of(integers(), none())))
    def test_drops_every_none(self, elements: list[int | None]) -> None:
        """No None survives, and a second pass has nothing left to drop."""
        kept = optional.values(*elements)

        assert None not in kept
        assert optional.values(*kept) == kept

    @given(lists(integers()))
    def test_keeps_present_values_in_order(self, elements: list[int]) -> None:
        """Nothing is missing, so everything comes back as it went in."""
        assert optional.values(*elements) == elements
