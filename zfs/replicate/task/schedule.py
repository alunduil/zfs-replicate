"""Ordering and dispatch for replication tasks.

:func:`~zfs.replicate.task.generate.generate` emits tasks in an order that
satisfies ZFS, but the order alone doesn't say which tasks may overlap. This
module states the constraints as a dependency graph, then runs that graph on a
bounded pool so independent data sets replicate at the same time.

Two kinds of edge build the graph. Within a data set, every task follows the
one before it, so a create precedes its sends, incremental sends stay in
order, and destroys never race sends against a single receive target. A data
set is serial internally; the parallelism is across data sets. Across data
sets, a parent's create precedes its children's creates, and every destroy of
a descendant precedes the destroy of the ancestor filesystem, which
``zfs destroy -r`` would otherwise take with it.

A run replicating ``tank/a``, its child ``tank/a/sub``, and ``tank/b`` to
``backup``, where ``tank/a`` and ``tank/a/sub`` are new to the destination and
``backup/tank/c`` no longer exists locally::

    CREATE backup/tank/a ------> SEND tank/a@1 --> SEND tank/a@2
      |
      v
    CREATE backup/tank/a/sub --> SEND tank/a/sub@1

    SEND tank/b@1 --> SEND tank/b@2

    DESTROY backup/tank/c/nested@1 --> DESTROY backup/tank/c/nested
                                         |
                                         v
    DESTROY backup/tank/c@1 ----------> DESTROY backup/tank/c

Every task with no incoming edge starts at once, up to the job limit; the rest
start as their own dependencies finish rather than at a batch boundary. So a
run takes about as long as its slowest single data set rather than the sum
across all of them, bounded by the job limit and by whatever share of the work
sits in one data set.
"""

import logging
from concurrent import futures
from typing import Callable, Dict, List, Optional, Set, Tuple

from .. import optional
from ..filesystem import FileSystem, remote_filesystem
from .type import Action, Task

logger = logging.getLogger(__name__)


def dependencies(remote: FileSystem, tasks: List[Task]) -> Dict[int, Set[int]]:
    """Map each task's index to the indices of the tasks it must follow."""
    groups: Dict[str, List[int]] = {}

    for index, task in enumerate(tasks):
        groups.setdefault(_data_set(remote, task), []).append(index)

    edges: Dict[int, Set[int]] = {index: set() for index in range(len(tasks))}

    for indices in groups.values():
        for earlier, later in zip(indices, indices[1:]):
            edges[later].add(earlier)

    _link_creates(tasks, groups, edges)
    _link_destroys(tasks, groups, edges)

    return edges


def dispatch(
    tasks: List[Task],
    edges: Dict[int, Set[int]],
    run: Callable[[Task], None],
    jobs: int,
) -> List[Tuple[Task, BaseException]]:
    """Run ``tasks`` on ``jobs`` threads in ``edges`` order and return the failures.

    A task starts once every task it depends on has finished. One that raises
    leaves the rest of the graph running and its own dependents unrun, so an
    unreachable data set costs the others nothing; the caller decides what a
    non-empty return means for the exit status.
    """
    blockers = {index: set(edges[index]) for index in range(len(tasks))}
    dependents: Dict[int, Set[int]] = {index: set() for index in range(len(tasks))}

    for index, blocking in blockers.items():
        for blocker in blocking:
            dependents[blocker].add(index)

    failures: List[Tuple[Task, BaseException]] = []

    with futures.ThreadPoolExecutor(max_workers=jobs) as executor:
        pending: Dict["futures.Future[None]", int] = {}

        def submit(indices: Set[int]) -> None:
            # Submitting in index order means --jobs 1 replays generate's order
            # wherever the graph leaves the choice open.
            for index in sorted(indices):
                pending[executor.submit(run, tasks[index])] = index

        submit({index for index, blocking in blockers.items() if not blocking})

        while pending:
            done, _ = futures.wait(pending, return_when=futures.FIRST_COMPLETED)

            ready: Set[int] = set()

            for future in done:
                index = pending.pop(future)
                error = future.exception()

                if error is not None:
                    failures.append((tasks[index], error))
                    _abandon(index, tasks, dependents)
                    continue

                for dependent in dependents[index]:
                    blockers[dependent].discard(index)

                    if not blockers[dependent]:
                        ready.add(dependent)

            submit(ready)

    return failures


def _data_set(remote: FileSystem, task: Task) -> str:
    # generate keys a SEND by the remote root rather than by its destination,
    # so a send's data set comes from its snapshot instead of its task key.
    if task.action is Action.SEND:
        return remote_filesystem(remote, optional.value(task.snapshot).filesystem).name

    return task.filesystem.name


def _link_creates(tasks: List[Task], groups: Dict[str, List[int]], edges: Dict[int, Set[int]]) -> None:
    creates = {
        name: index for name, indices in groups.items() for index in indices if tasks[index].action is Action.CREATE
    }

    for name, index in creates.items():
        ancestor = _nearest(name, creates)

        if ancestor is not None:
            edges[index].add(creates[ancestor])


def _link_destroys(tasks: List[Task], groups: Dict[str, List[int]], edges: Dict[int, Set[int]]) -> None:
    def destroys(indices: List[int]) -> List[int]:
        return [index for index in indices if tasks[index].action is Action.DESTROY]

    # Only the filesystem destroy takes descendants with it, so a data set's
    # snapshot destroys constrain nothing outside their own data set.
    roots = {
        name: index for name, indices in groups.items() for index in destroys(indices) if tasks[index].snapshot is None
    }

    for name, indices in groups.items():
        last = destroys(indices)

        if not last:
            continue

        ancestor = _nearest(name, roots)

        if ancestor is not None:
            edges[roots[ancestor]].add(last[-1])


def _nearest(name: str, candidates: Dict[str, int]) -> Optional[str]:
    parts = name.split("/")

    for depth in range(len(parts) - 1, 0, -1):
        ancestor = "/".join(parts[:depth])

        if ancestor in candidates:
            return ancestor

    return None


def _abandon(index: int, tasks: List[Task], dependents: Dict[int, Set[int]]) -> None:
    # Nothing to unschedule: a failed task never clears itself from its
    # dependents' blockers, so they are already unreachable. This only tells
    # the operator which data sets the failure took with it.
    abandoned: Set[int] = set()
    frontier = set(dependents[index])

    while frontier:
        current = frontier.pop()

        if current in abandoned:
            continue

        abandoned.add(current)
        frontier |= dependents[current]

    for skipped in sorted(abandoned):
        logger.warning("skipping %s: a task it depends on failed", _label(tasks[skipped]))


def _label(task: Task) -> str:
    action = task.action.name.lower()

    if task.snapshot is None:
        return f"{action} {task.filesystem.name}"

    return f"{action} {task.snapshot.filesystem.name}@{task.snapshot.name}"
