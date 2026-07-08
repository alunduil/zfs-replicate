# Test-generation conventions (AI agents)

Full conventions live in [`docs/reference/testing.md`](../../docs/reference/testing.md) -
that document is the single source of truth for humans and agents alike. Read it first.

## Shorthand for AI agents

When generating a test:

- Mirror the source path with a `_test` suffix on every segment:
  `zfs/replicate/foo/bar.py` -> `zfs_test/replicate_test/foo_test/bar_test.py`,
  with an `__init__.py` in each test package directory.
- Import the module under test as `sut`; end every `assert` with `# nosec`.
- For domain-shaped functions use `hypothesis.given` with strategies from the
  package's `strategies.py`; reserve fixed inputs for a specific literal case.
- Mock the process boundary (`zfs.replicate.process.open` / `process.run`),
  never real `zfs`/`ssh`. Test command builders on `Command.argv`/`.render()`.
- Drive the command line with `click.testing.CliRunner`.
- On a bug fix, add a regression test naming the issue in its docstring.

For the rationale, the worked examples, and the exceptions, follow the
`docs/reference/testing.md` link above; don't duplicate them here.
