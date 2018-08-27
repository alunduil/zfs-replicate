"""Types for Tasks."""

from enum import Enum, auto
from typing import NamedTuple as namedtuple

from ..filesystem import FileSystem


class Action(Enum):
    """Task Action."""

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


Task = namedtuple("Task", [("filesystem", FileSystem)])  # pylint: disable=invalid-name
