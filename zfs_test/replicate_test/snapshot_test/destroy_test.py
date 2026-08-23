"""zfs.replicate.snapshot.destroy tests."""

import subprocess

import pytest

from zfs.replicate import process
from zfs.replicate.command import Command
from zfs.replicate.error import ZFSReplicateError
from zfs.replicate.filesystem.type import filesystem

# zfs.replicate.snapshot re-exports destroy, so the package attribute is the function,
# not the module -- import the function rather than aliasing the module as sut.
from zfs.replicate.snapshot.destroy import destroy
from zfs.replicate.snapshot.type import Snapshot

SSH = Command.with_empty_env("ssh", "host")
SNAPSHOT = Snapshot(filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0)


def _fails_with(monkeypatch: pytest.MonkeyPatch, error: bytes) -> None:
    def fake_run(command: Command, **_kwargs: object) -> "subprocess.CompletedProcess[bytes]":
        return subprocess.CompletedProcess(command.argv, 1, b"", error)

    monkeypatch.setattr(process, "run", fake_run)


def test_destroy_reports_stderr_without_line_endings(monkeypatch: pytest.MonkeyPatch) -> None:
    """A failed destroy names the reason without the shell's trailing line ending."""
    _fails_with(monkeypatch, b"could not find any snapshots to destroy\r\n")

    with pytest.raises(ZFSReplicateError) as raised:
        destroy(SNAPSHOT, SSH)

    assert "could not find any snapshots to destroy" in raised.value.message
    assert "\\r" not in raised.value.message
    assert "\\n" not in raised.value.message


def test_destroy_reports_stderr_without_the_none_cipher_warning(monkeypatch: pytest.MonkeyPatch) -> None:
    """The ssh banner does not ride along into a failed destroy's message."""
    _fails_with(monkeypatch, b"WARNING: ENABLED NONE CIPHERcould not find any snapshots to destroy")

    with pytest.raises(ZFSReplicateError) as raised:
        destroy(SNAPSHOT, SSH)

    assert "could not find any snapshots to destroy" in raised.value.message
    assert "NONE CIPHER" not in raised.value.message
