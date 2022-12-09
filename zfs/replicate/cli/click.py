"""ZFS Replication Options."""
from enum import Enum
from typing import Any

import click


class EnumChoice(click.Choice):  # type: ignore[misc]
    """Choice made from Enum."""

    # Need any due to the private member only existing on subclasses through
    # what I assume is a metaclass construction.
    def __init__(self, enum: Any) -> None:
        """Construct EnumChoice from Enum."""
        self.__enum = enum

        choices = [x.lower() for x in enum._member_names_]

        super().__init__(list(sorted(set(choices))))

    def convert(self, value: Any, param: Any, ctx: Any) -> Any:
        """Convert string value to Enum value."""
        if isinstance(value, Enum):
            value = super().convert(value.name.lower(), param, ctx)
        else:
            value = super().convert(value.lower(), param, ctx)

        return next(x for x in self.__enum if x.name.lower() == value.lower())
