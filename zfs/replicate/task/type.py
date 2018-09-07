"""Types for Tasks."""

from enum import Enum, auto
from typing import NamedTuple, Optional

from ..filesystem import FileSystem
from ..snapshot import Snapshot


class Action(Enum):
    """Task Action."""

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


class Task(NamedTuple):
    """Task Type."""

    action: Action
    filesystem: FileSystem
    snapshot: Optional[Snapshot]
