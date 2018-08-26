"""SSH Cipher Type."""

from enum import Enum


class Cipher(Enum):
    """SSH Cipher."""

    DISABLED = 0
    STANDARD = 1
    FAST = 2
