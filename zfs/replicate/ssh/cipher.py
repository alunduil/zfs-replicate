# -*- coding: utf-8 -*-
"""SSH Cipher Type."""
from enum import auto
from enum import Enum


class Cipher(Enum):
    """SSH Cipher."""

    DISABLED = auto()
    STANDARD = auto()
    FAST = auto()
