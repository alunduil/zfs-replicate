# 1. Schedule parallel replication with a task dependency graph

## Status

Proposed

## Context and Problem Statement

`task/execute.py` replicates filesystems one at a time. For an operator
with many independent data sets, a run takes the sum of the times for
every data set rather than the time of the slowest one (issue #394, and
the original request in #3). The data sets are independent: their send
and receive pipes share no ordering constraint.

The tasks reaching `execute()` still carry ordering constraints, but the
current code encodes them by accident. `generate.py` emits a `CREATE`
task keyed by the destination filesystem and a run of `SEND` tasks keyed
by the remote root, so the create for a single data set and its sends
land in separate groups. A sort by name depth then happens to order
create before send. That accidental ordering is what breaks under
unguarded parallelism: a `SEND` can start before its `CREATE` finishes.

The question is how to run independent data sets concurrently while
honoring the ordering that the depth sort now hides.

## Decision Drivers

* Correctness: the create for a data set precedes its sends, a child's
  create follows its parent's, the incremental sends for a data set stay
  in order, and a child's destroy precedes its parent's.
* `--jobs 1` reproduces today's sequential behavior.
* One failed data set leaves the others running, and the run exits
  nonzero at the end.
* The smallest amount of custom concurrency code that stays correct, so
  the sequential and parallel paths stay testable.
* A ready task starts as soon as its own dependencies finish, not at a
  batch boundary.

## Considered Options

* A -- Self-scheduling queue, readiness carried on the message
* B -- Coordinator thread with separate ready and done queues
* C -- Main-thread dispatcher over a bounded `ThreadPoolExecutor`
* D -- Topological levels with a barrier between each

## Decision Outcome

Chosen option: **C, a main-thread dispatcher over a bounded
`ThreadPoolExecutor`**, because it states the ordering constraints as an
explicit dependency graph and lets the executor and `futures.wait` stand
in for a hand-built ready queue, which drops the coordinator thread, the
poison-pill shutdown, and the shared-state locking that the other
options carry.

An edge builder derives the graph from the tasks once, up front, from
four rules: create-before-send within a data set,
parent-create-before-child-create, send-before-send along the snapshot
chain for a data set, and child-destroy-before-parent-destroy. A `SEND`
task takes its data set identity from its snapshot rather than its task
key, since `generate.py` keys sends by the remote root.

The dispatcher runs on the main thread. It submits every task whose
in-degree is zero, waits for the first future to finish, decrements the
in-degree of that task's dependents, and submits any that reach zero. It
repeats until no futures remain. `ThreadPoolExecutor(max_workers=N)` from
`--jobs N` bounds concurrency, and `--jobs 1` collapses the graph to one
valid topological order that reproduces the sequential path.

### Consequences

* Good: the ordering rules become explicit data instead of a fragile
  sort key, so a reader sees why each task waits.
* Good: independent data sets run as soon as they're ready, up to the
  job limit, with no batch barrier.
* Good: all graph bookkeeping stays on the main thread, so no lock
  guards the shared state, and the loop ends when the last future
  drains, without poison pills.
* Bad: the edge builder and dispatcher add code and test surface that
  the sequential loop never needed.
* Neutral: the depth sort that ordered tasks before goes away, replaced
  by the explicit edges.

### Confirmation

Tests cover the sequential path (`--jobs 1`) and the parallel path
(`--jobs 4`) reaching the same final state, and a failing data set that
leaves the others to finish while the run exits nonzero (the issue #394
acceptance criteria).

## Pros and Cons of the Options

### A -- Self-scheduling queue, readiness on the message

Every task sits on one queue. A worker pulls a task, checks whether its
dependencies have finished, runs it when they have, and pushes it back
when they haven't.

* Good: one queue, little structure.
* Bad: a not-ready task cycles through the queue again and again, which
  burns cycles and needs a delay to avoid a busy spin.
* Bad: workers share the completion state, so a lock guards it.
* Bad: ordering emerges from the churn rather than from a stated rule,
  which is hard to reason about.

### B -- Coordinator thread with ready and done queues

A coordinator owns the in-degree map. It pushes ready tasks to a ready
queue, workers run them and report identifiers back on a done queue, and
the coordinator drains the done queue to release dependents.

* Good: workers hold no state, and one place owns the graph.
* Bad: a long-lived coordinator thread, two queues, and poison-pill
  shutdown add moving parts.
* Bad: the "queue empty but work still in flight" termination check is a
  classic place to get shutdown subtly wrong.

This sits close to the shape the issue implicitly describes -- the
current sequential loop with a coordinator around it -- but option C
reaches the same behavior without the extra thread or the queues.

### C -- Main-thread dispatcher over a bounded `ThreadPoolExecutor`

The chosen option. The executor's internal queue serves as the ready
holding area, and `futures.wait(FIRST_COMPLETED)` serves as the waiter,
so the dispatcher stays a single main-thread loop.

* Good: no coordinator thread, no hand-built queue, no poison pills.
* Good: graph mutation stays on the main thread, so no lock.
* Good: `--jobs 1` gives a bounded pool of one and a strict topological
  order without extra work.
* Bad: failure handling -- skipping the dependents of a failed task --
  lives in the loop rather than in a framework.

### D -- Topological levels with a barrier

Group tasks into dependency levels and run each level as a batch,
waiting for the whole level before the next starts.

* Good: the fewest moving parts, one `executor.map` per level.
* Bad: the barrier wastes parallelism, since a task that depends on one
  fast neighbor still waits for the slowest task in its level.

## More Information

Issue #394 carries the acceptance criteria and the `--jobs` flag. The
dependency on operational output flowing through the logging module
(issue #434) landed first, so log records stay one line under
concurrency.
