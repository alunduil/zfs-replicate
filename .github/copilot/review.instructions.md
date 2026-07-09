# Pull request review checklist

Apply this on every pull request so first-pass feedback stays consistent across
reviewers, human or AI. Treat each item as a question to answer, not a box to
tick -- when a rule doesn't fit a change, say why. The conventions come from
[`CONTRIBUTING.md`](../../CONTRIBUTING.md) and
[`docs/reference/testing.md`](../../docs/reference/testing.md); this file is the
reviewer's shorthand.

## Correctness

- A test exercises the behavior change and fails without it. A bug fix carries a
  regression test naming the scenario -- see
  [`test-generation.instructions.md`](test-generation.instructions.md).
- The diff meets the linked issue's acceptance criteria and stays within that
  issue's scope, with no unrelated refactors riding along.
- The change covers its edge cases: empty input, a missing optional argument, a
  remote or process that fails partway.

## Safety

- No new `shell=True` and no user input interpolated into a shell string. Every
  process runs through `zfs/replicate/process.py`, which starts each command
  from an `argv` list with `shell=False`.
- No new `# type: ignore` without a scoped error code and an inline reason the
  suppression can't be avoided (an `untyped` third-party stub, say), as in
  [`cli/log.py`](../../zfs/replicate/cli/log.py) and
  [`cli/click.py`](../../zfs/replicate/cli/click.py).
- No new `# noqa` or `# nosec` without the same code scope and inline reason.

## Typing

- Annotations match the surrounding style -- `typing` generics with `Optional`
  and `Union`, not the newer `list[int]` or `X | None` forms. Issue
  [#501](https://github.com/alunduil/zfs-replicate/issues/501) tracks that
  migration; the `X | None` union form stays out until the Python floor rises
  past 3.9, so don't introduce it ahead of that.
- No unjustified `Any`. When a value stays genuinely dynamic, a comment says
  why.
- New domain types use `@dataclass(frozen=True)`, matching
  [`command.py`](../../zfs/replicate/command.py) and the `type.py` modules.

## Process discipline

- Every process runs through `zfs/replicate/process.py` (`process.open` or
  `process.run`) -- the one place the shell-free guarantee lives. Code that
  builds a command line produces a `Command` (an `argv` list), never a joined
  string.

## Logging

- Operational output flows through a `logging.getLogger(__name__)` logger, not
  `print`. Reserve `click.echo` for the command line's own user-facing result,
  not library-level operational messages. See
  [`cli/log.py`](../../zfs/replicate/cli/log.py): library modules emit, and that
  module presents.

## Command line

- New or changed options update the `--help` text, and it reads well.
- Defaults preserve backward compatibility unless the pull request declares a
  breaking change (`!` in the commit subject plus a `BREAKING CHANGE:` footer,
  per [CONTRIBUTING.md](../../CONTRIBUTING.md#breaking-changes)).

## Docs

- A new user-visible capability updates the `README`. Rationale a reader can't
  infer from the code lands in the right layer under `docs/`, not just the pull
  request description.
- A reader who didn't write the change reviews the `--help` text for clarity.

## Test plan

- The pull request states a test plan concrete enough for the reviewer to run
  locally and observe the claimed result.
