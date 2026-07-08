# Test-generation conventions (AI agents)

Tests run under `pytest` with `--doctest-modules --cov=zfs` (see
[`pyproject.toml`](../../pyproject.toml)). The tree lives under `zfs_test/`.
When you add code, add tests that mirror the patterns already in that tree
rather than inventing new ones.

## Layout

- Test files mirror the source path with a `_test` suffix on every segment:
  `zfs/replicate/foo/bar.py` -> `zfs_test/replicate_test/foo_test/bar_test.py`.
- Give each test package directory an `__init__.py` (some older directories
  still lack one; follow the convention, don't copy the gaps).
- Import the module under test as `sut`, not by re-exporting its symbols:
  `import zfs.replicate.foo.bar as sut`.
- `assert` statements carry a `# nosec` comment; bandit flags bare asserts and
  this is the audited exception.

## Property tests

Prefer `hypothesis.given` with strategies over fixed inputs for functions whose
domain has shape (snapshots, filesystems, timestamps). Reserve fixed inputs for
cases where a specific literal is the point (a hostile string, a boundary flag).

- Shared strategies live in `zfs_test/replicate_test/<pkg>_test/strategies.py`
  and are imported by the tests in that package.
- A typical property test:

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

`zfs/replicate/process.py` is the one place the project spawns a process
(`process.open` for streaming/pipelines, `process.run` for run-to-completion).
Never let a test shell out to real `zfs` or `ssh`.

- Pure command *builders* (`*/command.py`) take no process; test them by
  asserting on the returned `Command.argv` and `Command.render()` directly.
- Code that *runs* a command mocks the boundary. Patch `zfs.replicate.process.run`
  (or `.open`) with `monkeypatch.setattr` and return a fake
  `subprocess.CompletedProcess` / `Popen`, so no external binary is invoked.

## CLI tests

Exercise the command line through `click.testing.CliRunner`. Patch the
collaborators the command dispatches to, such as `snapshot.list` and
`task.execute`, with `monkeypatch.setattr` and assert on `result.exit_code`,
`result.output`, or captured arguments. A typical command-line test:

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

When you fix a bug, add a test that fails before the fix and passes after, and
link the issue in the test's docstring (for example, `Regression test for #123.`) so the
motivation survives the diff.
