"""zfs.replicate.task.schedule tests."""

import logging
import threading
from graphlib import CycleError
from typing import Callable, Dict, List, Optional, Set, Tuple

import pytest
from hypothesis import assume, given
from hypothesis.strategies import booleans, fixed_dictionaries, integers, tuples

import zfs.replicate.task.schedule as sut
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot
from zfs.replicate.task.generate import generate
from zfs.replicate.task.type import Action, Task

REMOTE = filesystem("backup")

# A parent, its child, and an unrelated sibling, so a graph built from these
# exercises the cross-data-set rules and not only the within-data-set chain.
DATA_SETS = ["tank/a", "tank/a/sub", "tank/b"]


def test_send_follows_its_data_sets_create() -> None:
    """A SEND waits for the CREATE of the data set its snapshot belongs to."""
    tasks = [_create("tank/a"), _send("tank/a", "s0")]

    assert _blockers(tasks) == {"send tank/a@s0": {"create backup/tank/a"}}


def test_sends_chain_within_a_data_set() -> None:
    """Incremental sends for one data set stay in the order generate emitted them."""
    tasks = [_send("tank/a", "s0"), _send("tank/a", "s1")]

    assert _blockers(tasks) == {"send tank/a@s1": {"send tank/a@s0"}}


def test_sends_to_different_data_sets_are_independent() -> None:
    """Sends keyed by the same remote root still separate by their snapshots."""
    tasks = [_send("tank/a", "s0"), _send("tank/b", "s0")]

    # The raw graph rather than _blockers: dispatch seeds its first submission
    # from the tasks whose entry is empty, so every task needs an entry.
    assert sut.dependencies(REMOTE, tasks) == {0: set(), 1: set()}


def test_child_create_follows_parent_create() -> None:
    """A child data set can't be created before its parent exists."""
    tasks = [_create("tank/a"), _create("tank/a/sub")]

    assert _blockers(tasks) == {"create backup/tank/a/sub": {"create backup/tank/a"}}


def test_destroy_precedes_send_within_a_data_set() -> None:
    """A data set's destroys and sends never overlap against one receive target."""
    tasks = [_destroy("tank/a", "old"), _send("tank/a", "s0")]

    assert _blockers(tasks) == {"send tank/a@s0": {"destroy backup/tank/a@old"}}


def test_filesystem_destroy_follows_every_descendant_destroy() -> None:
    """``zfs destroy -r`` on a parent would take a descendant's snapshots with it."""
    tasks = [
        _destroy("tank/a", "s0"),
        _destroy("tank/a"),
        _destroy("tank/a/sub", "s0"),
        _destroy("tank/a/sub"),
    ]

    assert _blockers(tasks) == {
        "destroy backup/tank/a": {"destroy backup/tank/a@s0", "destroy backup/tank/a/sub"},
        "destroy backup/tank/a/sub": {"destroy backup/tank/a/sub@s0"},
    }


def test_failure_leaves_independent_data_sets_running() -> None:
    """A failed data set doesn't cost an unrelated one its replication."""
    tasks = [_create("tank/a"), _send("tank/a", "s0"), _create("tank/b"), _send("tank/b", "s0")]

    started, failures = _record(tasks, jobs=4, failing=tasks[0])

    assert sorted(started) == [0, 2, 3]
    assert [task for task, _ in failures] == [tasks[0]]


def test_dependents_of_a_failure_do_not_run() -> None:
    """Skipping is transitive: the child's send goes with the parent's create."""
    tasks = [_create("tank/a"), _create("tank/a/sub"), _send("tank/a/sub", "s0")]

    started, failures = _record(tasks, jobs=4, failing=tasks[0])

    assert started == [0]
    assert len(failures) == 1


def test_a_failure_is_reported_before_the_skips_it_explains() -> None:
    """The cause reaches the operator ahead of its consequences, not after the run drains."""
    tasks = [_create("tank/a"), _send("tank/a", "s0")]

    records = _captured(lambda: _record(tasks, jobs=1, failing=tasks[0]))

    assert [record.levelno for record in records] == [logging.ERROR, logging.WARNING]
    # Assert on the task each record names rather than its phrasing, so
    # rewording either message doesn't fail this.
    assert "create backup/tank/a" in records[0].getMessage()
    assert "send tank/a@s0" in records[1].getMessage()


def test_dispatch_surfaces_a_cyclic_graph() -> None:
    """A cycle surfaces instead of quietly replicating nothing, as it once did."""
    tasks = [_create("tank/a"), _create("tank/b")]

    with pytest.raises(CycleError):
        sut.dispatch(tasks, {0: {1}, 1: {0}}, lambda _task: None, jobs=1)


def test_dispatch_rejects_a_graph_that_skips_a_task() -> None:
    """A short graph is a bug rather than a smaller run, so it raises instead of replicating less."""
    tasks = [_send("tank/a", "s0"), _send("tank/b", "s0")]

    with pytest.raises(ValueError):
        sut.dispatch(tasks, {0: set()}, lambda _task: None, jobs=1)


@given(
    fixed_dictionaries({name: tuples(integers(0, 3), integers(0, 3)) for name in DATA_SETS}),
    booleans(),
)
def test_jobs_change_neither_the_work_nor_its_order(counts: Dict[str, Tuple[int, int]], follow_delete: bool) -> None:
    """`--jobs 1` and `--jobs 4` run every task once, each after its dependencies."""
    # generate raises KeyError when nothing is local; drop this once #452 lands.
    assume(any(local for local, _ in counts.values()))

    tasks = _generated(counts, follow_delete=follow_delete)
    edges = sut.dependencies(REMOTE, tasks)

    sequential, _ = _record(tasks, jobs=1)
    parallel, _ = _record(tasks, jobs=4)

    assert sorted(sequential) == list(range(len(tasks)))
    assert sorted(parallel) == list(range(len(tasks)))
    assert _respects(sequential, edges)
    assert _respects(parallel, edges)


def _captured(during: Callable[[], object]) -> List[logging.LogRecord]:
    """Return the records the scheduler logged while ``during`` ran, in order.

    Reading the module's own logger rather than ``caplog`` keeps the assertion
    on emission order clear of whatever handlers and levels the rest of the
    suite has left on the root logger.
    """
    records: List[logging.LogRecord] = []

    class _Recorder(logging.Handler):
        def emit(self, record: logging.LogRecord) -> None:
            records.append(record)

    logger = logging.getLogger(sut.__name__)
    handler = _Recorder()
    level = logger.level

    logger.addHandler(handler)
    logger.setLevel(logging.WARNING)

    try:
        during()
    finally:
        logger.setLevel(level)
        logger.removeHandler(handler)

    return records


def _record(
    tasks: List[Task],
    jobs: int,
    failing: Optional[Task] = None,
) -> Tuple[List[int], List[Tuple[Task, BaseException]]]:
    """Dispatch ``tasks`` and return the order they started in with the failures.

    A task starts only once everything it depends on has finished, so start
    order is a topological order of the graph whatever ``jobs`` is.
    """
    # Task is an unhashable dataclass, so identity keys the position map.
    positions = {id(task): index for index, task in enumerate(tasks)}
    started: List[int] = []
    lock = threading.Lock()

    def run(task: Task) -> None:
        with lock:
            started.append(positions[id(task)])

        if task is failing:
            raise RuntimeError("failing task")

    failures = sut.dispatch(tasks, sut.dependencies(REMOTE, tasks), run, jobs=jobs)

    return started, failures


def _blockers(tasks: List[Task]) -> Dict[str, Set[str]]:
    """Name what blocks each blocked task, so an expectation doesn't count positions.

    Tasks nothing blocks are left out.
    """
    labels = [_task_label(task) for task in tasks]
    assert len(set(labels)) == len(labels), "labels key the expectation, so they have to be unique"

    edges = sut.dependencies(REMOTE, tasks)

    return {labels[index]: {labels[blocker] for blocker in blocking} for index, blocking in edges.items() if blocking}


def _task_label(task: Task) -> str:
    """Name a task the way the tests talk about it.

    Deliberately its own formatter rather than schedule's, so rewording an
    operator-facing message can't quietly rewrite what these tests assert.
    """
    action = task.action.name.lower()

    if task.snapshot is None:
        return f"{action} {task.filesystem.name}"

    return f"{action} {task.snapshot.filesystem.name}@{task.snapshot.name}"


def _respects(order: List[int], edges: Dict[int, Set[int]]) -> bool:
    places = {index: place for place, index in enumerate(order)}

    return all(places[blocker] < places[index] for index, blockers in edges.items() for blocker in blockers)


def _generated(counts: Dict[str, Tuple[int, int]], follow_delete: bool) -> List[Task]:
    local = {filesystem(name): _snapshots(name, count) for name, (count, _) in counts.items() if count}
    remote = {
        filesystem(f"backup/{name}"): _snapshots(f"backup/{name}", count)
        for name, (_, count) in counts.items()
        if count
    }

    return generate(REMOTE, local, remote, follow_delete=follow_delete)


def _snapshots(name: str, count: int) -> List[Snapshot]:
    snapshots: List[Snapshot] = []

    for index in range(count):
        snapshots.append(_snapshot(name, f"s{index}", previous=snapshots[-1] if snapshots else None, timestamp=index))

    return snapshots


def _snapshot(name: str, snapshot: str, previous: Optional[Snapshot] = None, timestamp: int = 0) -> Snapshot:
    return Snapshot(filesystem=filesystem(name), name=snapshot, previous=previous, timestamp=timestamp)


def _create(name: str) -> Task:
    return Task(action=Action.CREATE, filesystem=filesystem(f"backup/{name}"), snapshot=None)


def _send(name: str, snapshot: str) -> Task:
    return Task(action=Action.SEND, filesystem=REMOTE, snapshot=_snapshot(name, snapshot))


def _destroy(name: str, snapshot: Optional[str] = None) -> Task:
    return Task(
        action=Action.DESTROY,
        filesystem=filesystem(f"backup/{name}"),
        snapshot=None if snapshot is None else _snapshot(f"backup/{name}", snapshot),
    )
