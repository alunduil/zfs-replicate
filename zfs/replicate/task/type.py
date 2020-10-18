# -*- coding: utf-8 -*-
"""Types for Tasks."""
from enum import auto
from enum import Enum
from typing import NamedTuple
from typing import Optional

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
