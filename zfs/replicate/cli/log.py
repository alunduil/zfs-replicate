"""Logging setup for the zfs-replicate command line.

Operational output flows through the ``zfs.replicate`` logger to standard
error. On a terminal it keeps click-log's colored ``level:`` presentation; off
one -- most importantly a systemd unit's journal stream -- each line is
prefixed with its sd-daemon priority so ``journalctl -p`` can filter by level.

The library modules only emit through ``logging.getLogger(__name__)``; this
module owns how that output is presented, which is the command line's concern.
"""

import logging
import sys

import click_log

logger = logging.getLogger("zfs.replicate")

# Python level -> sd-daemon(3) / syslog priority.
_PRIORITIES = {
    logging.CRITICAL: 2,
    logging.ERROR: 3,
    logging.WARNING: 4,
    logging.INFO: 6,
    logging.DEBUG: 7,
}


def _stderr_is_tty() -> bool:
    return sys.stderr.isatty()


# click-log's ColorFormatter is untyped, so mypy sees an ``Any`` base class.
class _Formatter(click_log.ColorFormatter):  # type: ignore[misc]
    """Colored output on a terminal; ``<N>`` priority prefixes otherwise.

    On a plain (non-terminal) stream -- most importantly the journal stream of
    a systemd unit -- each line is prefixed with its sd-daemon priority so
    journald records the level and ``journalctl -p`` can filter by it. On a
    terminal the record keeps click-log's colored ``level:`` presentation.
    """

    def format(self, record: logging.LogRecord) -> str:
        if _stderr_is_tty():
            return super().format(record)  # type: ignore[no-any-return]

        priority = _PRIORITIES.get(record.levelno, _PRIORITIES[logging.INFO])
        return "\n".join(
            f"<{priority}>{line}" for line in record.getMessage().splitlines()
        )


# The --verbosity/-v option, wired to the zfs.replicate logger. Default WARNING
# keeps a plain run quiet; click-log's own default is INFO. Read qualified as
# ``log.option``.
option = click_log.simple_verbosity_option(logger, default="WARNING")


def configure() -> None:
    """Route the ``zfs.replicate`` logger to standard error."""
    click_log.basic_config(logger)
    logger.handlers[0].setFormatter(_Formatter())
