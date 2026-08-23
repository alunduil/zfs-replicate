# Testing conventions

The conventions the `zfs_test/` suite follows. Tests run under `pytest` with
`--doctest-modules --cov=zfs --cov-report=term-missing` (configured in
[`pyproject.toml`](../../pyproject.toml)); `testpaths` is `zfs_test`.

`pytest-randomly` shuffles the order on every run, so a test that depends on
one before it fails somewhere. Pass `-p no:randomly` to hold the declared order
while reproducing such a failure. `pytest-xdist` is installed but stays out of
`addopts`, since `-n auto` spends more on worker startup than it saves at this
size; ask for it when a run is slow enough to pay for the workers.

## Layout

- A test module mirrors the module under test, with `_test` appended to every
  path segment, directories and file alike.
- Each test package directory carries an `__init__.py`. Two older directories
  (`cli_test/`, `task_test/`) predate this and lack one; new directories add it.
- A module's tests group into one class per exported symbol of the module under
  test, named for that symbol: `TestOverSsh` covers `over_ssh`, and
  `TestOptionsToFlags` covers `Options.to_flags`.
- A private helper's tests live in the class of the public symbol it serves.
  The parsing helpers behind `snapshot.list` are tested under `TestList`, and
  the assembly helpers behind `snapshot.send` under `TestSend`, so a module's
  class list reads as its public surface. The method name says which helper the
  test reaches for.

## Imports and assertions

- The module under test is imported as `sut`:
  `import zfs.replicate.foo.bar as sut`. Tests reach into it through that alias
  rather than re-exporting its symbols.
- `assert` statements need no marker. `ruff` ignores `S101` (assert-used)
  under `zfs_test/` through `per-file-ignores`, so test code asserts directly.

## Fixtures

A collaborator the test never asserts on is injected. A value the test asserts
against is written in the test, so the input and the expectation stay side by
side. That's why the `ssh` command arrives as the `ssh_command` fixture
wherever it merely has to exist, and as a literal in
`replicate_test/command_test.py`, where the wrapping is the subject of the
test.

A fixture lives in the class that uses it, and moves to
[`zfs_test/conftest.py`](../../zfs_test/conftest.py) once a third module needs
it. Setup that varies per test comes back as a function: the fixture closes
over the injected collaborators and returns a callable the test calls with the
one input it varies, as `assemble` and `capture_spawns` do in
`snapshot_test/send_test.py`.

```python
class TestDestroy:
    @pytest.fixture
    def snapshot(self) -> Snapshot:
        """Name the remote snapshot a destroy targets."""
        return Snapshot(filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0)

    def test_reports_stderr_without_line_endings(
        self,
        mocker: MockerFixture,
        ssh_command: Command,
        snapshot: Snapshot,
    ) -> None:
        """A failed destroy names the reason without the shell's trailing line ending."""
        _fails_with(mocker, b"could not find any snapshots to destroy\r\n")

        with pytest.raises(ZFSReplicateError) as raised:
            destroy(snapshot, ssh_command)

        assert "could not find any snapshots to destroy" in raised.value.message
```

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

A `@given` test takes no fixtures. A function-scoped fixture resolves once
while `@given` runs the body many times over fresh examples, which Hypothesis
reports as a `function_scoped_fixture` health check failure. Generated tests
therefore take their inputs from strategies alone, and the fixtures in the same
class serve its example-based tests. No test in the suite suppresses that
health check.

```python
from hypothesis import given
from hypothesis.strategies import lists

from zfs.replicate.snapshot.list import _snapshots
from zfs.replicate.snapshot.type import Snapshot
from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS


class TestList:
    """``list`` reads ``zfs list`` output back into snapshots."""

    @given(lists(SNAPSHOTS))
    def test_snapshots(self, snapshots: list[Snapshot]) -> None:
        """Round-trip the rendered list back through the parser."""
        output = "\n".join(f"{s.filesystem.name}@{s.name}\t{s.timestamp}" for s in snapshots)
        assert _snapshots(output.encode()) == snapshots
```

## The process boundary

[`zfs/replicate/process.py`](../../zfs/replicate/process.py) is the sole place
the project spawns a process: `process.open` for streaming, `process.pipeline`
for chaining one command's output into the next, and `process.run` for
run-to-completion. Tests never spawn real `zfs` or `ssh`.

- Command *builders* (`*/command.py`) construct a `Command` and spawn nothing.
  Their tests assert on `Command.argv` and `Command.render()` directly.
- Code that *runs* a command patches the boundary. A test replaces
  `zfs.replicate.process.run` (or `process.open`) through the `mocker` fixture,
  returning a fake `subprocess.CompletedProcess` or `Popen`, so no external
  binary runs. Patching `process.open` also covers `process.pipeline`, which
  spawns each of its stages through it.

## Command-line tests

The command line is exercised through `click.testing.CliRunner`. A fixture
patches the collaborators a command dispatches to, such as `snapshot.list` and
`task.execute`, and hands the test the stub it asserts against. Assertions read
`result.exit_code`, `result.output`, or the arguments a stub recorded.

```python
import zfs.replicate.cli.main as sut
from click.testing import CliRunner

# Every invocation needs a destination and a key file; only the flags under test vary.
CONNECTION = ["-l", "alunduil", "-i", "pyproject.toml", "example.com", "bogus", "bogus"]


class TestMain:
    """``main`` parses the command line and hands the run to ``task.execute``."""

    @pytest.fixture
    def invoke(self) -> Callable[..., Result]:
        """Invoke the command line with the connection arguments every run needs."""

        def _invoke(*options: str) -> Result:
            return CliRunner().invoke(sut.main, [*options, *CONNECTION])

        return _invoke

    def test_set_rejects_malformed_property(self, invoke: Callable[..., Result]) -> None:
        """`--receive-set` without an equals sign is rejected before execution."""
        result = invoke("--receive-set", "readonly")

        assert result.exit_code != 0
        assert "KEY=VALUE" in result.output
```

## Regression tests

A bug fix adds a test that fails before the fix and passes after. Its docstring
describes the behavior or scenario under test and links the issue, not the issue
alone (for example, `Rejects a snapshot name containing '@'; see #123.`).
