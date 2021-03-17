"""Types for Tasks."""
from dataclasses import dataclass
from enum import Enum, auto
from typing import Optional

from ..filesystem import FileSystem
from ..snapshot import Snapshot


class Action(Enum):
    """Task Action."""

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


@dataclass
class Task:
    """Task Type."""

    action: Action
    filesystem: FileSystem
    snapshot: Optional[Snapshot]
