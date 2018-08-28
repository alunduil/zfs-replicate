"""ZFS Replication Options."""

from typing import Any

import click
import stringcase


class EnumChoice(click.Choice):
    """Choice made from Enum."""

    # Need any due to the private member only existing on subclasses through
    # what I assume is a metaclass construction.
    def __init__(self, enum: Any) -> None:
        self.__enum = enum

        choices = [x.lower() for x in enum._member_names_]  # pylint: disable=protected-access
        super().__init__(list(sorted(set(choices))))

    def convert(self, value: Any, param: Any, ctx: Any) -> Any:
        value = super().convert(value.name.lower(), param, ctx)

        return next(x for x in self.__enum if x.name.lower() == value.lower())

    def get_metavar(self, param: Any) -> str:
        metavar = stringcase.snakecase(self.__enum.__name__)  # type: str

        if metavar.endswith("_enum"):
            metavar = metavar[:-5]

        return metavar.upper()
