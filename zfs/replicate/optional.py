"""Optional Functions."""

from typing import List, Optional, TypeVar

Value = TypeVar("Value")


def value(optional: Optional[Value]) -> Value:
    """Raise error if optional is None."""
    if optional is None:
        raise RuntimeError("unexpected None")

    return optional


def values(*optionals: Optional[Value]) -> List[Value]:
    """Keep the values that are present, in the order given.

    >>> values("zfs send", None, "zfs receive")
    ['zfs send', 'zfs receive']
    """
    return [optional for optional in optionals if optional is not None]
