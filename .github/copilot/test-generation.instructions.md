# Test-generation conventions (agents)

Rules for generating a test. They're self-contained; consult
[`docs/reference/testing.md`](../../docs/reference/testing.md) for rationale,
worked examples, and exceptions when a rule here doesn't fit the case.

- Test module mirrors the module under test, with `_test` appended to every
  path segment; `__init__.py` in each test package directory.
- Import the module under test as `sut`; end every `assert` with `# nosec`.
- Checkable property over a shaped domain (round-trip, invariant, bound):
  `hypothesis.given` + strategies from the package's `strategies.py`; pin a
  must-run input with `@example`, don't split it into a separate test.
- No property, or generation has cost/side effects (exact argv/render output, a
  spawned process): plain fixed-input test, no Hypothesis.
- Mock the process boundary `zfs.replicate.process.open`/`.run`, never real
  `zfs`/`ssh`. Test command builders on `Command.argv`/`.render()`.
- Command line: `click.testing.CliRunner`.
- Bug fix: add a regression test naming the issue in its docstring.
