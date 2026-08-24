"""Optional Functions."""

from typing import TypeVar

Value = TypeVar("Value")


def value(optional: Value | None) -> Value:
    """Raise error if optional is None."""
    if optional is None:
        raise RuntimeError("unexpected None")

    return optional


def values(*optionals: Value | None) -> list[Value]:
    """Keep the values that are present, in the order given.

    >>> values(1, None, 3)
    [1, 3]
    """
    return [optional for optional in optionals if optional is not None]
