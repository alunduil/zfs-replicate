"""Task Execution."""

import itertools
import logging
from typing import List, Tuple

from .. import filesystem, optional, receive, send, snapshot
from ..command import Command
from ..compress import Compression
from ..filesystem import FileSystem
from .type import Action, Task

logger = logging.getLogger(__name__)


def execute(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    tasks: List[Tuple[FileSystem, List[Task]]],
    ssh_command: Command,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
) -> None:
    """Execute all tasks."""
    sorted_tasks = sorted(tasks, key=lambda x: len(x[0].name.split("/")), reverse=True)

    for _, filesystem_tasks in sorted_tasks:
        action_tasks = {
            action: list(action_tasks)
            for action, action_tasks in itertools.groupby(filesystem_tasks, key=lambda x: x.action)
        }

        for action, a_tasks in action_tasks.items():
            if action == Action.CREATE:
                _create(a_tasks, ssh_command=ssh_command)
            elif action == Action.DESTROY:
                _destroy(a_tasks, ssh_command=ssh_command)
            elif action == Action.SEND:
                _send(
                    remote,
                    a_tasks,
                    ssh_command=ssh_command,
                    compression=compression,
                    send_options=send_options,
                    receive_options=receive_options,
                )


def _create(tasks: List[Task], ssh_command: Command) -> None:
    for task in tasks:
        logger.info("creating filesystem %s", task.filesystem.name)
        filesystem.create(task.filesystem, ssh_command=ssh_command)


def _destroy(tasks: List[Task], ssh_command: Command) -> None:
    for task in tasks:
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
    tasks: List[Task],
    ssh_command: Command,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
) -> None:
    for task in tasks:
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
