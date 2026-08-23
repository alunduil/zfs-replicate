"""Tests for zfs.replicate.optional."""

from typing import List, Optional

from hypothesis import given
from hypothesis.strategies import integers, lists, none, one_of

from zfs.replicate import optional


class TestValue:
    """``value`` unwraps an optional that must be present."""

    def test_none(self) -> None:
        """optional.value(None) → RuntimeError."""
        # Use try except due to lack of typing on pytest module.
        try:
            optional.value(None)
        except RuntimeError:
            pass
        except:  # noqa: E722
            raise AssertionError("Expected RuntimeError") from None

    @given(integers())
    def test_not_none(self, value: int) -> None:
        """optional.value(value) == value."""
        assert optional.value(value) == value


class TestValues:
    """``values`` keeps the present members of a sequence of optionals."""

    @given(lists(one_of(integers(), none())))
    def test_drops_every_none(self, elements: List[Optional[int]]) -> None:
        """No None survives, and a second pass has nothing left to drop."""
        kept = optional.values(*elements)

        assert None not in kept
        assert optional.values(*kept) == kept

    @given(lists(integers()))
    def test_keeps_present_values_in_order(self, elements: List[int]) -> None:
        """Nothing is missing, so everything comes back as it went in."""
        assert optional.values(*elements) == elements
