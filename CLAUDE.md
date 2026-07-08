# Project guide for Claude Code

This file gives Claude Code (and similar coding agents) the
project-local context it needs to land changes that fit the
conventions here. Two friction patterns surfaced during a
recent `/insights` review and motivated this file (see issue
#445): agents reaching for `curl` and first-principles
scripts when local tools already cover the task, and agents
bleeding scope across linked or sibling issues.

## Tool inventory

The first move when picking up a task should be discovering
what's already wired into the project, not reinventing it.

- **Package manager:** `Poetry`. `pyproject.toml` is the
  source of truth; `poetry.lock` pins the resolved versions;
  `poetry.toml` carries `Poetry` settings.
- **Development shell:** `Nix` via `shell.nix` (which imports
  `nix/shell.nix`). `.envrc` activates the shell through
  `direnv` on directory change. `.devcontainer/` covers
  Visual Studio Code and GitHub `Codespaces`;
  `.devcontainer/post-create.sh` bootstraps the container.
- **Tests:** `pytest` with `--doctest-modules --cov=zfs
  --cov-report=term-missing` (configured under
  `[tool.pytest.ini_options]` in `pyproject.toml`). The test
  tree lives under `zfs_test/`. `Hypothesis` is available as
  a development dependency.
- **Lint and format:** `pre-commit` orchestrates the local
  hooks. Inside `.pre-commit-config.yaml` you find `black`,
  `isort`, `flake8` (`.flake8`), `bandit`, `pylint`
  (`.pylintrc`), `pydocstyle`, and `mypy` (`mypy.ini`).
  Prose lint runs through `Vale` (`.vale.ini` and
  `styles/`).
- **Entry point:** the command-line tool installs as
  `zfs-replicate = "zfs.replicate.cli.main:main"` (see
  `[tool.poetry.scripts]` in `pyproject.toml`).

Use these before hand-rolling alternatives. If a task feels
as though it needs `curl`, a manual API call, or a one-off
`Bash` helper, check the inventory first; the local tool that
already covers it produces less churn for reviewers.

## Python conventions

Concrete rules for the Python written here. These are the
current defaults, not aspirations; confirm against a
neighbouring module before assuming.

- **Typing:** old-style `typing` generics (`Optional[X]`,
  `List[X]`, `Mapping[K, V]`, `Tuple[...]`). The floor is
  Python 3.9 (`requires-python = ">=3.9"`), so PEP 604
  `X | None` unions and bare `list[X]` generics are out.
- **Running commands:** build a `Command` (frozen argv
  dataclass, `zfs/replicate/command.py`, via
  `Command.with_empty_env`) and run it through `process.open`
  / `process.run`, which exec with `shell=False`. Never a
  shell string or `shell=True`. Remote commands quote through
  the `over_ssh` helper (`shlex.join`), not hand-placed
  quotes.
- **Domain types:** `@dataclass(frozen=True)`, in the
  per-package `type.py` modules.
- **Errors:** raise subclasses of `ZFSReplicateError`
  (`zfs/replicate/error.py`), which extends
  `click.ClickException`.
- **Output:** library modules emit through
  `logging.getLogger(__name__)`; the application logger is
  `logging.getLogger("zfs.replicate")` (`cli/log.py`). Use
  `click.echo` only for CLI results printed to stdout.
- **Tests:** mirror `zfs/replicate/...` under
  `zfs_test/replicate_test/...` with a `_test.py` suffix.
  Hypothesis strategies live in the sibling
  `<pkg>_test/strategies.py`.

## Scope discipline

When working from numbered issues:

- Before opening a pull request, confirm the scope doesn't
  overlap with linked or sibling issues. If it might, ask in
  the issue thread instead of guessing.
- If an issue is blocked by an unreleased prerequisite, propose
  deferral with a `blocked-by` edge rather than writing
  premature code that will need to be reworked.
- Revert any out-of-scope incidental edits before requesting
  review. One concern per pull request.

Most of this is encoded in the `issue-work` skill already;
this note exists so that Claude applies the discipline
without having to call the skill explicitly.

## Why this project specifically

GitHub lists the open release milestones, and the active
`blocked` label signals that sequencing matters here. Clear
scope discipline avoids prematurely shipping something blocked
by an earlier milestone. The Python tool stack is rich enough (`black`,
`isort`, `flake8`, `bandit`, `pylint`, `pydocstyle`, `mypy`,
all gated through `pre-commit`) that an agent can plausibly
reinvent any one of those checks unless the inventory is in
front of it.

## Session defaults

`.claude/settings.json` ships committed Claude Code defaults
so agent sessions start productive. A `SessionStart` hook runs
`poetry install` and `pre-commit install`, mirroring
`.devcontainer/post-create.sh`, so the environment and git
hooks are ready before the first turn. The `permissions.allow`
list pre-approves the inspection commands an agent reaches for
early: `poetry run pytest`, `pre-commit run`, `poetry run
zfs-replicate --help`, and `git diff`, `git log`, `git
status`, and `git show`. Machine-specific overrides go in
`.claude/settings.local.json`, which `.gitignore` keeps out of
version control.
