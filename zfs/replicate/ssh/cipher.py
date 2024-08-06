"""SSH Cipher Type."""

from enum import Enum, auto


class Cipher(Enum):
    """SSH Cipher."""

    DISABLED = auto()
    STANDARD = auto()
    FAST = auto()
