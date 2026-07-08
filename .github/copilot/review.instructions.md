# Pull request review checklist

Measure every zfs-replicate pull request against this shared bar, whether the
reviewer is a person or an AI assistant (Copilot review, Claude Code `/review`
and `/code-review`). Apply it to the diff, not the whole tree: flag what the
change introduces, not pre-existing debt it sits beside.

Conventions this leans on live in [`CONTRIBUTING.md`](../../CONTRIBUTING.md).
That document is the source of truth when the two disagree.

## Correctness

- Confirm new behaviour has tests under `zfs_test/` that fail without the change.
- Confirm the change satisfies the linked issue's acceptance criteria.
- Weigh the edge cases: empty input, a missing snapshot, an SSH or remote error,
  a half-drained pipeline, and nonzero subprocess exit codes.

## Safety

- Reject any new `shell=True`, and any user input interpolated into a shell
  string.
- Require each new `# type: ignore` to name a specific error code
  (`# type: ignore[code]`) and explain itself; refuse a blanket `# type: ignore`.
  Hold `# nosec`, `# noqa`, and `pylint: disable` to the same bar.

## Typing

- Require PEP 604 unions (`X | None`) over `typing.Optional` or `typing.Union`.
- Reject `Any` that lacks a justifying comment; keep `mypy --strict` green.
- Require new domain types to be `@dataclass(frozen=True)`.

## Process boundary

- Route every child process through
  [`zfs/replicate/process.py`](../../zfs/replicate/process.py), the one audited
  `subprocess` boundary (argv list, `shell=False`). Nothing else imports
  `subprocess` or calls `Popen`/`run` directly.

## Logging

- Send operational output through `logging.getLogger(__name__)`, never `print`
  or a bare `click.echo`. The one sanctioned `click.echo` writes the final task
  report to stdout; everything else is logging.

## Command line

- Update `--help` output for any new or changed option.
- Preserve backward-compatible defaults unless the PR is explicitly a breaking
  change (`!` marker plus a `BREAKING CHANGE:` footer -- see
  [CONTRIBUTING.md](../../CONTRIBUTING.md#breaking-changes)).

## Docs

- Update the README for any new user-visible capability.
- Read the `--help` text as someone who hasn't seen the option before, and
  confirm it reads well.

## Test plan

- Confirm the PR states how the author verified the change, and that you can run
  that plan locally (`poetry run pytest`, `pre-commit run --all-files`).
