"""zfs.replicate.cli.log tests."""

import logging

from pytest_mock import MockerFixture

import zfs.replicate.cli.log as sut

# Module-private, but exercised directly here rather than through the whole
# logging stack.
Formatter = sut._Formatter  # pylint: disable=protected-access


def _record(level: int, message: str) -> logging.LogRecord:
    return logging.LogRecord("zfs.replicate", level, __file__, 0, message, None, None)


class TestConfigure:
    """``configure`` installs the formatter that presents operational output.

    That formatter, ``_Formatter``, decides the presentation and is exercised
    here without routing a whole logging stack through it.
    """

    def test_formatter_prefixes_priority_off_tty(self, mocker: MockerFixture) -> None:
        """Off a terminal, each line carries its sd-daemon priority for journald."""
        mocker.patch.object(sut, "_stderr_is_tty", return_value=False)

        assert Formatter().format(_record(logging.ERROR, "boom")) == "<3>boom"
        assert Formatter().format(_record(logging.INFO, "a\nb")) == "<6>a\n<6>b"

    def test_formatter_colors_on_tty(self, mocker: MockerFixture) -> None:
        """On a terminal, click-log's colored ``level:`` presentation is kept."""
        mocker.patch.object(sut, "_stderr_is_tty", return_value=True)

        formatted = Formatter().format(_record(logging.ERROR, "boom"))

        assert not formatted.startswith("<")
        assert "error: " in formatted
