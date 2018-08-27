"""Optional Functions."""

from typing import Optional, TypeVar

Value = TypeVar("Value")


def value(optional: Optional[Value]) -> Value:
    """Raise error if optional is None."""

    if optional is None:
        raise RuntimeError("unexpected None")

    return optional
