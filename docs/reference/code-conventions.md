# Code conventions

The conventions the `zfs/` package follows where `ruff` and `mypy` leave the
choice open. Formatting, import order, and typing style are settled by the
configuration in [`pyproject.toml`](../../pyproject.toml) and reported by
`pre-commit run --all-files`, so they're out of scope here. Conventions for the
test suite live in [testing.md](testing.md).

## Package layout

- A package under `zfs/replicate/` owns one subject: a ZFS noun (`filesystem`,
  `snapshot`), a side of the stream (`send`, `receive`), or a concern that
  crosses both (`ssh`, `compress`, `task`).
- `type.py` holds the package's domain types and `command.py` its `Command`
  builders. Every other module is named for the operation it performs, matching
  the `zfs` subcommand where there is one: `list.py`, `create.py`,
  `destroy.py`, `send.py`.
- `__init__.py` re-exports the package's public surface. `mypy` runs strict, so
  an implicit re-export fails: a name is either aliased to itself
  (`from .create import create as create`) or listed in `__all__`.
- A name that would shadow a built-in inside its own module takes another name
  in the module and gets renamed at the re-export. `snapshot.list` is
  `list_snapshots` in
  [`snapshot/list.py`](../../zfs/replicate/snapshot/list.py).

## Domain types

- A type on a package's public surface is a frozen dataclass in that package's
  `type.py`. A helper type used by one module sits in that module, as
  `Pipeline` does in
  [`snapshot/send.py`](../../zfs/replicate/snapshot/send.py).
- A closed set of choices is an `Enum`. `EnumChoice` in
  [`cli/click.py`](../../zfs/replicate/cli/click.py) puts one on the command
  line, so the enum stays the only list of accepted values.

## Commands and processes

- A command is a [`Command`](../../zfs/replicate/command.py): a program and its
  argument list, built through `Command.with_empty_env`. A command is never
  assembled as a shell string.
- [`process.py`](../../zfs/replicate/process.py) is the only module that spawns
  a process, through `open` for streaming, `pipeline` for chaining stages, or
  `run` for a command that runs to completion. Each one execs the argument list
  with `shell=False`.
- `Command.render` quotes a command into a string with `shlex.join`, for the
  remote login shell that `ssh` hands its arguments to. `command.over_ssh`,
  which wraps commands to run through `ssh`, is its only caller.

## Errors

A failure the operator has to see raises `ZFSReplicateError` from
[`error.py`](../../zfs/replicate/error.py). It inherits `click.ClickException`,
so `click` prints the message and exits nonzero with no traceback. The
constructor takes that message followed by any context arguments, and shows the
message alone.

## Output

- Operational progress goes to the logger, which writes to standard error. A
  library module emits through `logging.getLogger(__name__)`;
  [`cli/log.py`](../../zfs/replicate/cli/log.py) owns the `zfs.replicate`
  logger those propagate to, along with the `--verbosity` option and the
  formatting.
- `click.echo` carries a command's result to standard output and nothing else.
  The `--dry-run` plan is its only use.

## Command-line options

- A global option is a `click.option` decorator on `main` in
  [`cli/main.py`](../../zfs/replicate/cli/main.py). Its `help=` text is what
  `--help` prints.
- The `--send-*` and `--receive-*` flags belong to the `send_group` and
  `receive_group` decorators in
  [`cli/options.py`](../../zfs/replicate/cli/options.py), which collapse each
  group into a single `send.Options` or `receive.Options` argument.
