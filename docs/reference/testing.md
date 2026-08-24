# Testing conventions

The conventions the `zfs_test/` suite follows. Tests run under `pytest` with
`--doctest-modules --cov=zfs --cov-report=term-missing` (configured in
[`pyproject.toml`](../../pyproject.toml)); `testpaths` is `zfs_test`.

Test order is randomized, so pass `-p no:randomly` to reproduce a failure that
only appears in some runs.

## Layout

- A test module mirrors the module under test, with `_test` appended to every
  path segment, directories and file alike.
- Each test package directory carries an `__init__.py`. Two older directories
  (`cli_test/`, `task_test/`) predate this and lack one; new directories add it.
- A module's tests group into one class per exported symbol, named for it.
- A private helper's tests live in the class of the public symbol it serves, so
  a module's class list reads as its public surface. `snapshot.list`'s parsing
  helpers are tested under `TestList`, and `snapshot.send`'s assembly helpers
  under `TestSend`. The method name says which helper a test reaches for.
- A class docstring says what its tests hold the symbol to, not what the symbol
  does. The symbol's own docstring covers that, and a paraphrase here drifts
  from it.

## Imports and assertions

- The module under test is imported as `sut`:
  `import zfs.replicate.foo.bar as sut`. Tests reach into it through that alias
  rather than re-exporting its symbols.
- `assert` statements need no marker. `ruff` ignores `S101` (assert-used)
  under `zfs_test/` through `per-file-ignores`, so test code asserts directly.

## Fixtures

Inject a collaborator the test never asserts on. Write a value the test
asserts against into the test itself, so the input and the expectation stay
side by side. The `ssh` command shows both: it arrives as the `ssh_command`
fixture wherever it merely has to exist, and as a literal in
`replicate_test/command_test.py`, where the tests check how it gets wrapped.

A fixture lives in the class that uses it, and moves to
[`zfs_test/conftest.py`](../../zfs_test/conftest.py) once a second module needs
the same setup.

Setup that varies per test comes back as a function: the fixture closes over
its collaborators and returns a callable, as `fails_with`, `assemble`, and
`capture_spawns` do.

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

A `@given` test takes no fixtures; the fixtures in its class serve the
example-based tests beside it.

```python
from hypothesis import given
from hypothesis.strategies import lists

from zfs.replicate.snapshot.list import _snapshots
from zfs.replicate.snapshot.type import Snapshot
from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS


class TestList:
    """Rendered ``zfs list`` output parses back to the snapshots it came from."""

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
`task.execute`, and hands back the stub the test asserts against. Assertions
read `result.exit_code`, `result.output`, or the arguments that stub recorded.

## Regression tests

A bug fix adds a test that fails before the fix and passes after. Its docstring
describes the behavior or scenario under test and links the issue, not the issue
alone (for example, `Rejects a snapshot name containing '@'; see #123.`).

## Mutation testing

A nightly job scores the suite by mutation testing, configured under
`[tool.mutmut]` in [`pyproject.toml`](../../pyproject.toml). A mutant that
survives is killed by a new test, recorded as an equivalent mutant in a comment
at the code, or excluded with `# pragma: no mutate block` and the reason no test
can reach it.

`mutmut` runs the suite several times in one process, and `pytest` builds a
fresh instance of a test class for each pass. Hypothesis reads those instances
as a property test called from differing executors, so
[`zfs_test/conftest.py`](../../zfs_test/conftest.py) suppresses that health
check while `mutmut` drives the suite. A property test needs nothing of its own,
and an ordinary `pytest` run leaves the check on.
