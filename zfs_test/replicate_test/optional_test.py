"""Tests for zfs.replicate.optional."""
from hypothesis import given
from hypothesis.strategies import integers

from zfs.replicate import optional


def test_value_none() -> None:
    """optional.value(None) → RuntimeError."""
    # Use try except due to lack of typing on pytest module.
    try:
        optional.value(None)
    except RuntimeError:
        pass
    except:  # noqa: E722 # pylint: disable=W0702
        assert False, "Expected RuntimeError"  # nosec


@given(integers())  # type: ignore[misc]
def test_value_not_none(value: int) -> None:
    """optional.value(value) == value."""
    assert optional.value(value) == value  # nosec
