"""ZFS FileySystem Type."""
from dataclasses import dataclass


@dataclass(order=True, frozen=True)
class FileSystem:
    """FileSystem Type."""

    dataset: str
    name: str
    readonly: bool


def filesystem(name: str, readonly: bool = False) -> FileSystem:
    """Create a FileSystem from a name."""
    dataset = name.split("/", 1)[0]

    return FileSystem(dataset=dataset, name=name, readonly=readonly)
