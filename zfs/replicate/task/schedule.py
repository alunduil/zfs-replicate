"""Ordering and dispatch for replication tasks.

:func:`~zfs.replicate.task.generate.generate` emits tasks in an order that
satisfies ZFS, but the order alone doesn't say which tasks may overlap. This
module states the constraints as a dependency graph, then runs that graph on a
bounded pool so independent data sets replicate at the same time.

A data set is serial internally; the parallelism is across data sets. Two
reasons for that sit outside any single rule: a data set's destroys must not
race its sends against one receive target, and a filesystem destroy must wait
for every descendant, since ``zfs destroy -r`` would otherwise take them with
it.

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
start as their own dependencies finish rather than at a batch boundary.
"""

import itertools
import logging
from concurrent import futures
from graphlib import TopologicalSorter
from typing import Callable, Dict, Iterator, List, Optional, Set, Tuple

from .. import optional
from ..filesystem import FileSystem, remote_filesystem
from .type import Action, Task

logger = logging.getLogger(__name__)


def dependencies(remote: FileSystem, tasks: List[Task]) -> Dict[int, Set[int]]:
    """Map each task's index to the indices of the tasks it must follow."""
    groups = _group_by_data_set(remote, tasks)
    edges: Dict[int, Set[int]] = {index: set() for index in range(len(tasks))}

    for earlier, later in itertools.chain(
        _chain_edges(groups),
        _create_edges(tasks, groups),
        _destroy_edges(tasks, groups),
    ):
        edges[later].add(earlier)

    return edges


def dispatch(
    tasks: List[Task],
    edges: Dict[int, Set[int]],
    run: Callable[[Task], None],
    jobs: int,
) -> List[Tuple[Task, BaseException]]:
    """Run ``tasks`` on ``jobs`` threads in ``edges`` order and return the failures.

    ``edges`` keys tasks by position, so it needs an entry for every task.
    A task starts once every task it depends on has finished. One that raises
    leaves the rest of the graph running and its own dependents unrun, so an
    unreachable data set costs the others nothing; the caller decides what a
    non-empty return means for the exit status.
    """
    return _Dispatcher(tasks, edges, run).run(jobs=jobs)


class _Dispatcher:
    """A single run of a task graph, tracking what has finished and what may start."""

    def __init__(self, tasks: List[Task], edges: Dict[int, Set[int]], run: Callable[[Task], None]) -> None:
        _require_edge_per_task(tasks, edges)

        self._tasks = tasks
        self._run = run
        # TopologicalSorter reads a mapping of node to predecessors, which is
        # what edges already is.
        self._sorter: TopologicalSorter[int] = TopologicalSorter(edges)
        self._dependents = _reverse(edges)
        self._pending: Dict["futures.Future[None]", int] = {}
        self._failures: List[Tuple[Task, BaseException]] = []

    def run(self, *, jobs: int) -> List[Tuple[Task, BaseException]]:
        """Drain the graph over a pool of ``jobs`` threads and return the failures."""
        self._sorter.prepare()

        with futures.ThreadPoolExecutor(max_workers=jobs) as executor:
            self._submit(executor, self._sorter.get_ready())

            while self._pending:
                done, _ = futures.wait(self._pending, return_when=futures.FIRST_COMPLETED)

                for future in done:
                    self._settle(future)

                # A failed task is never marked done, so its dependents stay out
                # of get_ready() and the pool draining is what ends the run.
                # is_active() would stay true for the rest of the process.
                self._submit(executor, self._sorter.get_ready())

        return self._failures

    def _submit(self, executor: futures.ThreadPoolExecutor, indices: Tuple[int, ...]) -> None:
        # Submitting in index order means --jobs 1 replays generate's order
        # wherever the graph leaves the choice open. get_ready() promises no
        # order of its own.
        for index in sorted(indices):
            self._pending[executor.submit(self._run, self._tasks[index])] = index

    def _settle(self, future: "futures.Future[None]") -> None:
        """Retire a finished task, releasing its dependents or abandoning them."""
        index = self._pending.pop(future)
        error = future.exception()

        if error is None:
            self._sorter.done(index)

            return

        # Reported here rather than by the caller so the cause reaches the
        # operator ahead of the skips it explains, instead of after every other
        # data set has drained.
        logger.error("%s failed: %s", _label(self._tasks[index]), error)
        self._failures.append((self._tasks[index], error))
        self._abandon(index)

    def _abandon(self, index: int) -> None:
        # Nothing to unschedule: a failed task is never marked done, so the
        # sorter never offers its dependents. This only tells the operator which
        # data sets the failure took with it.
        for skipped in sorted(self._downstream(index)):
            logger.warning("skipping %s: a task it depends on failed", _label(self._tasks[skipped]))

    def _downstream(self, index: int) -> Set[int]:
        """Return every task that follows ``index``, directly or through another."""
        reached: Set[int] = set()
        frontier = set(self._dependents[index])

        while frontier:
            current = frontier.pop()

            if current in reached:
                continue

            reached.add(current)
            frontier |= self._dependents[current]

        return reached


def _group_by_data_set(remote: FileSystem, tasks: List[Task]) -> Dict[str, List[int]]:
    """Map each data set to its task indices, in the order generate emitted them."""
    groups: Dict[str, List[int]] = {}

    for index, task in enumerate(tasks):
        groups.setdefault(_data_set(remote, task), []).append(index)

    return groups


def _chain_edges(groups: Dict[str, List[int]]) -> Iterator[Tuple[int, int]]:
    """Yield an edge from each task in a data set to the one after it."""
    for indices in groups.values():
        yield from itertools.pairwise(indices)


def _create_edges(tasks: List[Task], groups: Dict[str, List[int]]) -> Iterator[Tuple[int, int]]:
    """Yield an edge from each parent's create to the create of every child."""
    creates = _index_by_data_set(tasks, groups, lambda task: task.action is Action.CREATE)

    for name, index in creates.items():
        ancestor = _nearest(name, creates)

        if ancestor is not None:
            yield creates[ancestor], index


def _destroy_edges(tasks: List[Task], groups: Dict[str, List[int]]) -> Iterator[Tuple[int, int]]:
    """Yield an edge from each descendant's last destroy to the ancestor's filesystem destroy."""
    # Only the filesystem destroy takes descendants with it, so a data set's
    # snapshot destroys constrain nothing outside their own data set.
    roots = _index_by_data_set(tasks, groups, lambda task: task.action is Action.DESTROY and task.snapshot is None)

    for name, indices in groups.items():
        destroys = [index for index in indices if tasks[index].action is Action.DESTROY]

        if not destroys:
            continue

        ancestor = _nearest(name, roots)

        if ancestor is not None:
            yield destroys[-1], roots[ancestor]


def _index_by_data_set(
    tasks: List[Task],
    groups: Dict[str, List[int]],
    matches: Callable[[Task], bool],
) -> Dict[str, int]:
    """Map each data set to the last of its tasks satisfying ``matches``."""
    return {name: index for name, indices in groups.items() for index in indices if matches(tasks[index])}


def _require_edge_per_task(tasks: List[Task], edges: Dict[int, Set[int]]) -> None:
    """Reject a graph that doesn't key every task by its position in ``tasks``.

    Indices are all that tie the two together, and a task absent from ``edges``
    never reaches the sorter at all, so a short graph would leave data sets
    silently unreplicated.
    """
    if set(edges) != set(range(len(tasks))):
        raise ValueError("edges needs one entry per task, keyed by position in tasks")


def _reverse(edges: Dict[int, Set[int]]) -> Dict[int, Set[int]]:
    """Map each task's index to the indices of the tasks that follow it."""
    dependents: Dict[int, Set[int]] = {index: set() for index in edges}

    for index, blocking in edges.items():
        for blocker in blocking:
            dependents[blocker].add(index)

    return dependents


def _data_set(remote: FileSystem, task: Task) -> str:
    # generate keys a SEND by the remote root rather than by its destination,
    # so a send's data set comes from its snapshot instead of its task key.
    if task.action is Action.SEND:
        return remote_filesystem(remote, optional.value(task.snapshot).filesystem).name

    return task.filesystem.name


def _nearest(name: str, candidates: Dict[str, int]) -> Optional[str]:
    parts = name.split("/")

    for depth in range(len(parts) - 1, 0, -1):
        ancestor = "/".join(parts[:depth])

        if ancestor in candidates:
            return ancestor

    return None


def _label(task: Task) -> str:
    action = task.action.name.lower()

    if task.snapshot is None:
        return f"{action} {task.filesystem.name}"

    return f"{action} {task.snapshot.filesystem.name}@{task.snapshot.name}"
