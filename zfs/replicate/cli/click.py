"""ZFS Replication Options."""
from enum import Enum
from typing import Any

import click
import stringcase


class EnumChoice(click.Choice):
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

    def get_metavar(self, param: Any) -> str:
        """Use Enum name as metavar."""
        metavar: str = stringcase.snakecase(self.__enum.__name__)

        if metavar.endswith("_enum"):
            metavar = metavar[:-5]

        return metavar.upper()
