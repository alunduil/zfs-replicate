# Test-generation conventions (agents)

Rules for generating a test. They're self-contained; consult
[`docs/reference/testing.md`](../../docs/reference/testing.md) for rationale,
worked examples, and exceptions when a rule here doesn't fit the case.

- Test module mirrors the module under test, with `_test` appended to every
  path segment; `__init__.py` in each test package directory.
- Import the module under test as `sut`; end every `assert` with `# nosec`.
- Domain-shaped inputs: `hypothesis.given` + strategies from the package's
  `strategies.py`. Fixed inputs only when a specific literal is the subject.
- Mock the process boundary `zfs.replicate.process.open`/`.run`, never real
  `zfs`/`ssh`. Test command builders on `Command.argv`/`.render()`.
- Command line: `click.testing.CliRunner`.
- Bug fix: add a regression test naming the issue in its docstring.
