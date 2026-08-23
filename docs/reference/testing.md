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
- `assert` statements need no marker. `ruff` ignores `S101` (assert-used)
  under `zfs_test/` through `per-file-ignores`, so test code asserts directly.

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
  `zfs.replicate.process.run` (or `process.open`) with `monkeypatch.setattr`,
  returning a fake `subprocess.CompletedProcess` or `Popen`, so no external
  binary runs. Patching `process.open` also covers `process.pipeline`, which
  spawns each of its stages through it.

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
        ["--receive-set", "readonly", "-l", "alunduil", "-i", "pyproject.toml",
         "example.com", "bogus", "bogus"],
    )
    assert result.exit_code != 0
    assert "KEY=VALUE" in result.output
```

## Regression tests

A bug fix adds a test that fails before the fix and passes after. Its docstring
describes the behavior or scenario under test and links the issue, not the issue
alone (for example, `Rejects a snapshot name containing '@'; see #123.`).

## Mutation testing

Mutation testing measures whether the suite would notice if `zfs/` were wrong.
Line coverage only records that a line ran. `mutmut` rewrites `zfs/` one small
edit at a time, flipping a comparison or replacing a literal, and reruns the
suite against each edit. Each edit is a mutant. The suite kills a mutant that
makes it fail. A mutant it still passes survives, and names an assertion that
doesn't constrain the behavior it appears to cover.

The `Score mutant detection` job in
[`daily.yml`](../../.github/workflows/daily.yml) sweeps the tree nightly and
writes the tally to its run summary. It gates nothing.

### Run the sweep

```bash
poetry install --with mutation
poetry run mutmut run --max-children 1
poetry run mutmut results
```

`mutmut run` copies `zfs/` and `zfs_test/` into `mutants/`, builds the mutants,
and tests each one, which takes roughly a quarter of an hour for the whole tree.
`mutmut results` then lists everything that survived.

`--max-children 1` is required. The parallel runner mis-attributes child exit
codes on this suite, so a mutant that survives every time on its own reports as
killed on some runs and survived on others. Serial verdicts are reproducible.

`mutmut run` also takes names and globs, so
`mutmut run 'zfs.replicate.snapshot.*'` re-tests one package against the mutants
already built.

### Read a survivor

Take a name from `mutmut results` and read the edit it stands for:

```bash
poetry run mutmut show zfs.replicate.snapshot.list.x__snapshot__mutmut_4
```

Each diff is one of three things:

- **A gap in the suite.** The edit changes what a user would see and nothing
  caught it. Write a test that fails on the mutated behavior, then
  `mutmut run <name>` to confirm the mutant dies.
- **An equivalent mutant.** The edit can't change observable behavior, so no
  test can kill it. A message string a caller never reads and a `sorted()` call
  on an already-ordered sequence both land here. Leave it.
- **Out of reach.** `mutmut` activates a mutant when the mutated function runs.
  Code that runs while the module is first imported has already finished by
  then. `# pragma: no mutate block` on the enclosing function excludes it, with
  a comment giving the reason, as the two Click option groups in
  [`zfs/replicate/cli/options.py`](../../zfs/replicate/cli/options.py) do.

`mutmut browse` shows the same diffs in a terminal UI and re-tests a mutant in
place, which saves a round trip while writing the test that kills it.
