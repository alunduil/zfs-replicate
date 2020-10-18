# -*- coding: utf-8 -*-
"""ZFS Replication Stream Compression."""
from enum import auto
from enum import Enum


class Compression(Enum):
    """Stream Compression Choices."""

    LZ4 = auto()
