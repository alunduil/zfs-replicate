# 1. Schedule parallel replication with a task dependency graph

## Status

Accepted

## Context and Problem Statement

`task/execute.py` replicates filesystems one at a time, so a run's total
time is the sum across every data set. Independent data sets could run
concurrently -- their send and receive pipes share no ordering
constraint -- bounded by `--jobs N` and by the longest single data set,
since each stays serial internally (create before sends, sends in
order). Issue
[#394](https://github.com/alunduil/zfs-replicate/issues/394) targets
this, from the original request in
[#3](https://github.com/alunduil/zfs-replicate/issues/3).

The tasks reaching `execute()` still carry ordering constraints, but the
current code encodes them incidentally: `generate.py` keys a `CREATE` by
its destination filesystem and the following `SEND`s by the remote root,
so the create for a data set and its sends land in separate groups, and
only a sort by name depth places create before send. Unguarded
parallelism breaks that: a `SEND` can start before its `CREATE`
finishes.

## Decision Drivers

* Correctness: the create for a data set precedes its sends, a child's
  create follows its parent's, the incremental sends for a data set stay
  in order, and a child's destroy precedes its parent's.
* `--jobs 1` reproduces today's sequential behavior.
* One failed data set leaves the others running, and the run exits
  nonzero at the end.
* Least custom concurrency code that stays correct and testable.
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
* Bad: the edge builder and dispatcher add code and test surface that
  the sequential loop never needed.
* Neutral: the depth sort that ordered tasks before goes away, replaced
  by the explicit edges.

### Confirmation

Tests cover the sequential path (`--jobs 1`) and the parallel path
(`--jobs 4`) reaching the same final state, and a failing data set that
leaves the others to finish while the run exits nonzero (the
[#394](https://github.com/alunduil/zfs-replicate/issues/394) acceptance
criteria).

## Pros and Cons of the Options

### A -- Self-scheduling queue, readiness on the message

Every task sits on one queue. A worker pulls a task, checks whether its
dependencies have finished, runs it when they have, and pushes it back
when they haven't.

* Good: one queue, little structure.
* Bad: a not-ready task cycles back repeatedly until a delay throttles
  the spin.
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

The chosen option.

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

Issue [#394](https://github.com/alunduil/zfs-replicate/issues/394) has
the acceptance criteria. The dependency on operational output flowing
through the logging module (issue
[#434](https://github.com/alunduil/zfs-replicate/issues/434)) landed
first, so log records stay one line under concurrency.
