"""Task Execution."""

import logging

from .. import filesystem, receive, send, snapshot
from ..command import Command
from ..compress import Compression
from ..filesystem import FileSystem
from .type import CreateFilesystemTask, DestroyFilesystemTask, DestroySnapshotTask, SendSnapshotTask, Task

logger = logging.getLogger(__name__)


def execute(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    tasks: list[tuple[FileSystem, list[Task]]],
    *,
    ssh_command: Command,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
) -> None:
    """Execute all tasks."""
    sorted_tasks = sorted(tasks, key=lambda x: len(x[0].name.split("/")), reverse=True)

    for _, filesystem_tasks in sorted_tasks:
        for task in filesystem_tasks:
            match task:
                case CreateFilesystemTask():
                    _create(task, ssh_command=ssh_command)
                case DestroyFilesystemTask():
                    _destroy_filesystem(task, ssh_command=ssh_command)
                case DestroySnapshotTask():
                    _destroy_snapshot(task, ssh_command=ssh_command)
                case SendSnapshotTask():
                    _send(
                        remote,
                        task,
                        ssh_command=ssh_command,
                        compression=compression,
                        send_options=send_options,
                        receive_options=receive_options,
                    )


def _create(task: CreateFilesystemTask, ssh_command: Command) -> None:
    logger.info("creating filesystem %s", task.filesystem.name)
    filesystem.create(task.filesystem, ssh_command=ssh_command)


def _destroy_filesystem(task: DestroyFilesystemTask, ssh_command: Command) -> None:
    logger.info("destroying filesystem %s", task.filesystem.name)
    filesystem.destroy(task.filesystem, ssh_command=ssh_command)


def _destroy_snapshot(task: DestroySnapshotTask, ssh_command: Command) -> None:
    logger.info("destroying snapshot %s@%s", task.snapshot.filesystem.name, task.snapshot.name)
    snapshot.destroy(task.snapshot, ssh_command=ssh_command)


def _send(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    task: SendSnapshotTask,
    *,
    ssh_command: Command,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
) -> None:
    current = task.snapshot

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
