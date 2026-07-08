"""zfs.replicate.cli.log tests."""

import logging

import pytest

import zfs.replicate.cli.log as sut

# Module-private, but exercised directly here rather than through the whole
# logging stack.
Formatter = sut._Formatter  # pylint: disable=protected-access


def _record(level: int, message: str) -> logging.LogRecord:
    return logging.LogRecord("zfs.replicate", level, __file__, 0, message, None, None)


def test_formatter_prefixes_priority_off_tty(monkeypatch: pytest.MonkeyPatch) -> None:
    """Off a terminal, each line carries its sd-daemon priority for journald."""
    monkeypatch.setattr(sut, "_stderr_is_tty", lambda: False)

    assert Formatter().format(_record(logging.ERROR, "boom")) == "<3>boom"  # nosec
    assert Formatter().format(_record(logging.INFO, "a\nb")) == "<6>a\n<6>b"  # nosec


def test_formatter_colors_on_tty(monkeypatch: pytest.MonkeyPatch) -> None:
    """On a terminal, click-log's colored ``level:`` presentation is kept."""
    monkeypatch.setattr(sut, "_stderr_is_tty", lambda: True)

    formatted = Formatter().format(_record(logging.ERROR, "boom"))

    assert not formatted.startswith("<")  # nosec
    assert "error: " in formatted  # nosec
