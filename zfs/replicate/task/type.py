"""Types for Tasks."""

import collections

from enum import auto, Enum

class Action(Enum):
    """Task Action."""

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()

Task = collections.namedtuple("Task", [])
