"""Task Execution."""

import functools
import logging
from typing import List

from .. import filesystem, optional, receive, send, snapshot
from ..command import Command
from ..compress import Compression
from ..error import ZFSReplicateError
from ..filesystem import FileSystem
from . import schedule
from .type import Action, Task

logger = logging.getLogger(__name__)


def execute(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    tasks: List[Task],
    ssh_command: Command,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
    jobs: int = 1,
) -> None:
    """Execute all tasks, replicating up to ``jobs`` data sets at a time.

    Tasks run in the order :mod:`zfs.replicate.task.schedule` derives, and
    ``jobs`` of 1 runs them one after another. A failure doesn't stop the run:
    the data sets that don't depend on it finish, and the failures raise
    together at the end.
    """
    run = functools.partial(
        _run,
        remote,
        ssh_command=ssh_command,
        compression=compression,
        send_options=send_options,
        receive_options=receive_options,
    )

    failures = schedule.dispatch(tasks, schedule.dependencies(remote, tasks), run, jobs=jobs)

    if failures:
        for _, error in failures:
            logger.error("%s", error)

        raise ZFSReplicateError(f"{len(failures)} of {len(tasks)} replication tasks failed")


def _run(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    task: Task,
    ssh_command: Command,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
) -> None:
    if task.action is Action.CREATE:
        _create(task, ssh_command=ssh_command)
    elif task.action is Action.DESTROY:
        _destroy(task, ssh_command=ssh_command)
    elif task.action is Action.SEND:
        _send(
            remote,
            task,
            ssh_command=ssh_command,
            compression=compression,
            send_options=send_options,
            receive_options=receive_options,
        )


def _create(task: Task, ssh_command: Command) -> None:
    logger.info("creating filesystem %s", task.filesystem.name)
    filesystem.create(task.filesystem, ssh_command=ssh_command)


def _destroy(task: Task, ssh_command: Command) -> None:
    if task.snapshot is None:
        logger.info("destroying filesystem %s", task.filesystem.name)
        filesystem.destroy(task.filesystem, ssh_command=ssh_command)
    else:
        logger.info(
            "destroying snapshot %s@%s",
            task.snapshot.filesystem.name,
            task.snapshot.name,
        )
        snapshot.destroy(task.snapshot, ssh_command=ssh_command)


def _send(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    task: Task,
    ssh_command: Command,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
) -> None:
    current = optional.value(task.snapshot)

    logger.info("sending snapshot %s@%s", current.filesystem.name, current.name)
    snapshot.send(
        remote,
        current,
        ssh_command=ssh_command,
        compression=compression,
        send_options=send_options,
        receive_options=receive_options,
        previous=current.previous,
    )
    logger.debug("sent snapshot %s@%s", current.filesystem.name, current.name)
