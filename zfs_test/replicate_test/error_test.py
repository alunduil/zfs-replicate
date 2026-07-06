"""zfs.replicate.error tests."""

import logging

import pytest

import zfs.replicate.error as sut


def test_error_logs_before_propagating(caplog: pytest.LogCaptureFixture) -> None:
    """Constructing a ZFSReplicateError records the message at ERROR."""
    with caplog.at_level(logging.ERROR, logger="zfs.replicate"):
        error = sut.ZFSReplicateError("replication failed")

    assert error.message == "replication failed"  # nosec
    assert ("zfs.replicate.error", logging.ERROR, "replication failed") in [  # nosec
        (record.name, record.levelno, record.message) for record in caplog.records
    ]
