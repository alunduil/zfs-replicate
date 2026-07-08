# Testing conventions

The conventions the `zfs_test/` suite follows. Tests run under `pytest` with
`--doctest-modules --cov=zfs --cov-report=term-missing` (configured in
[`pyproject.toml`](../../pyproject.toml)); `testpaths` is `zfs_test`.

## Layout

- A test module mirrors the module under test, with `_test` appended to every
  path segment, directories and file alike.
- Each test package directory carries an `__init__.py`. Two older directories
  (`cli_test/`, `task_test/`) predate this and lack one; new directories add it.

## Imports and assertions

- The module under test is imported as `sut`:
  `import zfs.replicate.foo.bar as sut`. Tests reach into it through that alias
  rather than re-exporting its symbols.
- `assert` statements carry a trailing `# nosec`. Bandit flags bare asserts;
  this is the audited exception for test code.

## Property tests

A function with a checkable property over a shaped domain—a round-trip, an
invariant, a bound—uses `hypothesis.given` with strategies rather than
hand-picked inputs. A specific input that must always run (a regression, a known
edge) is pinned with `@example` on the property, not split into its own
fixed-input test.

Hypothesis is dropped where generation buys nothing: an assertion of exact
output for one input (a rendered command, an `argv` list), or an input whose
execution has cost or side effects (a spawned process). Those tests state their
inputs as literals.

Shared strategies for a package live in
`zfs_test/replicate_test/<pkg>_test/strategies.py` and are imported by that
package's tests.

```python
from hypothesis import given
from hypothesis.strategies import lists

from zfs.replicate.snapshot.list import _snapshots
from zfs.replicate.snapshot.type import Snapshot
from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS


@given(lists(SNAPSHOTS))  # type: ignore[misc]
def test_snapshots(snapshots: list[Snapshot]) -> None:
    """Round-trip the rendered list back through the parser."""
    output = "\n".join(f"{s.filesystem.name}@{s.name}\t{s.timestamp}" for s in snapshots)
    assert _snapshots(output.encode()) == snapshots  # nosec
```

## The process boundary

[`zfs/replicate/process.py`](../../zfs/replicate/process.py) is the sole place
the project spawns a process: `process.open` for streaming and pipeline wiring,
`process.run` for run-to-completion. Tests never spawn real `zfs` or `ssh`.

- Command *builders* (`*/command.py`) construct a `Command` and spawn nothing.
  Their tests assert on `Command.argv` and `Command.render()` directly.
- Code that *runs* a command patches the boundary. A test replaces
  `zfs.replicate.process.run` (or `process.open`) with `monkeypatch.setattr`,
  returning a fake `subprocess.CompletedProcess` or `Popen`, so no external
  binary runs.

## Command-line tests

The command line is exercised through `click.testing.CliRunner`. The
collaborators a command dispatches to—`snapshot.list`, `task.execute`, and the
like—are patched with `monkeypatch.setattr`; assertions read `result.exit_code`,
`result.output`, or the arguments the fakes captured.

```python
import zfs.replicate.cli.main as sut
from click.testing import CliRunner


def test_set_rejects_malformed_property() -> None:
    """`--receive-set` without an equals sign is rejected before execution."""
    result = CliRunner().invoke(
        sut.main,
        ["--receive-set", "readonly", "-l", "alunduil", "-i", "mypy.ini",
         "example.com", "bogus", "bogus"],
    )
    assert result.exit_code != 0  # nosec
    assert "KEY=VALUE" in result.output  # nosec
```

## Regression tests

A bug fix adds a test that fails before the fix and passes after. The test's
docstring names the issue it covers (for example, `Regression test for #123.`).
