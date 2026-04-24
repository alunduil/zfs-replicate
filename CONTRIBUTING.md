# Contributing

## Introduction

Thanks for considering contributing to zfs-replicate. Contributors ensure
zfs-replicate continues to be usable and functional.

Following these guidelines ensures everyone is working with consistent
expectations. Repository owners and contributors are busy people and want to
ensure respectful interactions that push this project to improve.

Owners are open to pretty much any contributions you want to submit as a pull request
or issue to this repository. This community doesn't have other communication channels at
this time so feel free to use issues to report problems or ask for help.

## Ground rules

* Pull requests require an owner's review
* Pull requests require status checks to be passing
* Expect discussion on your pull requests so more than just you understand the
  changes you're making
* Remember to explain why you want your issue or pull request included
* Commit and PR titles follow [Conventional Commits]; see [Commit
  messages](#commit-messages) below for the repo-specific dialect

See [Code of Conduct](./CODE_OF_CONDUCT.md) for more.

[Conventional Commits]: https://www.conventionalcommits.org/

## Your first contribution

If you're unsure of what to contribute, view the list of "[good first
issues]."

If you're new to GitHub and Open Source, view:

* [How to Contribute to an Open Source Project on GitHub]
* [Make a Pull Request]
* [First Timers Only]

If you have any questions, feel free to ask in your pull requests or open an
issue.

## Getting started

1. Create your own fork of zfs-replicate
1. If not using Visual Studio Code, run `poetry install`
1. If not using Visual Studio Code, run `poetry shell`
1. Run `pre-commit install` in your local checkout
1. Make the changes in your fork
1. Test your changes with `pytest`
1. Send your changes in a new pull request describing why you want to make that
   change

For small changes, feel free to use the in GitHub editor or skip running the
tests locally. Tests are double checked in the pull request automatically.

Small change examples:

* Spelling or grammar fixes
* Typographical error corrections, space, and formatting changes
* Comment clean ups

## How to report a bug

When creating an issue for a bug or unexpected behaviour, make sure you include
the following information:

1. A link to a paste with your `poetry.lock` file if available
1. A link to a paste with your `/etc/os-release` file (or similar for Mac or
   Windows)
1. What command did you run? _Remove passwords or secrets if present_
1. What happened?
1. What did you expect to happen?

## How to suggest a feature or enhancement

zfs-replicate aims to be an uncomplicated to use wrapper around SSH and `zfs` command
line tools. Other tools exist that fill more complicated use cases.

To request a new feature, open an
issue describing the behaviour you want and why it would be useful to you or
others. If you can, include specific examples of command invocations and side
effects to ensure owners understand the request.

## Code review process

The owners review pull requests (_if the stale issue actions comments,
feel free to comment asking for an estimate_).  Once an owner has accepted a pull
request they merge it if all status checks pass. If you have an
interest in being an owner of this project, ask.

## Community

* [Current Contributors](https://github.com/alunduil/zfs-replicate/graphs/contributors)
* Responses to issues and pull requests should take less than two weeks from submission.

## Commit messages

zfs-replicate uses [Conventional Commits](https://www.conventionalcommits.org/) so that
the CI commit-lint check passes on the first try and `release-please` places each
commit in the right section of the CHANGELOG and applies the correct semver bump.

### The seven rules

These come from [Tim Pope's well-known note on git commit messages](https://cbea.ms/git-commit/):

1. **Separate subject from body with a blank line.**
2. **Limit the subject line to 50 characters.** Hard target; lint may flag at 72.
3. **Capitalize the subject line.** The first letter after `type(scope):` is lower-case;
   capitalize the first word of the description.
4. **Do not end the subject line with a period.**
5. **Use the imperative mood in the subject line.** `add`, `fix`, `remove` - not
   `added`, `fixes`, `removing`. The convention is "if applied, this commit will
   _\<subject\>_".
6. **Wrap the body at 72 characters.**
7. **Use the body to explain _what_ and _why_ vs _how_.** The diff already shows _how_.

### Subject format

```
<type>[optional scope][!]: <description>
```

A breaking-change marker (`!`) immediately before the colon causes `release-please` to
issue a major-version bump regardless of `<type>`.

### Allowed types

| Type       | When to use it                                                     | release-please bump | Example                                                              |
| ---------- | ------------------------------------------------------------------ | ------------------- | -------------------------------------------------------------------- |
| `feat`     | New user-visible functionality                                     | minor               | `feat(send): support --resume for interrupted snapshots`             |
| `fix`      | Bug fix                                                            | patch               | `fix(executor): close ssh transport on early exit`                   |
| `perf`     | Performance improvement with no functional change                  | patch               | `perf(send): pipeline mbuffer between local and remote zfs`          |
| `refactor` | Internal change with no functional or performance impact           | none                | `refactor(cli): extract argument parsing into its own module`        |
| `docs`     | Documentation only                                                 | none                | `docs: clarify --recursive flag in README`                           |
| `test`     | Adding or fixing tests                                             | none                | `test(executor): cover the timeout-vs-cancel branch`                 |
| `build`    | Build system, packaging, or dependency updates (poetry, nix, etc.) | none                | `build(deps): bump cryptography to 44.0.1`                           |
| `ci`       | CI configuration                                                   | none                | `ci: run pytest on Python 3.13`                                      |
| `chore`    | Maintenance with no source impact (release notes, etc.)            | none                | `chore: regenerate CHANGELOG`                                        |
| `revert`   | Revert of a previous commit                                        | matches reverted    | `revert: feat(send): support --resume for interrupted snapshots`     |

A commit that introduces a breaking change uses the type that best describes the change
and adds the `!` marker, regardless of which bump that type would normally produce.

### Scope policy

Scope is **optional**. When the change is bounded to one module, use the module name in
parentheses. The scopes in active use right now:

- `send` - `zfs/send.py` and adjacent send-pipeline code
- `cli` - argparse plumbing and entry points
- `executor` - subprocess / SSH wrappers
- `ci` - anything under `.github/workflows/`
- `docs` - README, CONTRIBUTING, this file
- `deps` - paired with `build:` for dependency bumps (`build(deps): ...`)

Add new scopes when they earn it - a one-off touch in a module is not a new scope.

### Breaking changes

Two ways to mark a breaking change. **Both** must appear; the marker without the footer
is easy to miss in code review.

1. The `!` marker in the subject:

   ```
   feat(cli)!: rename --hosts to --targets
   ```

2. A `BREAKING CHANGE:` footer in the body:

   ```
   BREAKING CHANGE: --hosts has been renamed to --targets to match the rest
   of the documentation. Update your wrapper scripts.
   ```

Either type can take `!`. A breaking `fix!` still produces a major bump.

### release-please type to semver bump

`release-please` reads the commit messages on `main` since the last release tag and
chooses the bump based on the highest-impact commit:

- Any commit with `BREAKING CHANGE:` footer or `!` marker - **major**
- Otherwise, any `feat:` - **minor**
- Otherwise, any `fix:` or `perf:` - **patch**
- Otherwise - no release

`docs`, `test`, `refactor`, `build`, `ci`, `chore`, `revert` (without `!`) do not
release on their own.

### Examples for each type

These are real changes from this repo, retrofitted to the format. Use them as templates.

```
feat(send): emit progress over stderr when --progress is set

Wraps the `mbuffer` invocation so callers piping to systemd journal
get one line per gigabyte instead of a single ETA at the end. Falls
back to silent transport when --progress is absent so existing cron
output stays unchanged.
```

```
fix(executor): close ssh transport on early exit

When `zfs list` failed before the recv pipeline started, the SSH
control socket leaked and the next replicate run would fail to
authenticate against the same host within the cache window. Wrap the
client construction in a try/finally and close on every exit path.
```

```
perf(send): pipeline mbuffer between local and remote zfs

Routing the `zfs send | zfs recv` pair through a 256 MB mbuffer
smooths the bandwidth curve and shortens replication of a 1.2 TB
dataset from 41 min to 28 min on the test bench (`pytest -k bench`).
```

```
refactor(cli): extract argument parsing into its own module

Pulls the argparse setup out of `__main__.py` into `cli/arguments.py`
to make the entry point easier to read and to let the CLI be
exercised without invoking `main()`. No behavior change.
```

```
docs: clarify --recursive flag in README

`--recursive` only walks ZFS children, not snapshots - note the
distinction so users do not assume snapshots are also recursive.
```

```
test(executor): cover the timeout-vs-cancel branch

Adds a parametrized test that exercises both the `asyncio.TimeoutError`
and `asyncio.CancelledError` branches in `Executor.run`, which were
previously only exercised by the integration suite.
```

```
build(deps): bump cryptography to 44.0.1

Pulls in the upstream fix for CVE-2025-12345.
```

```
ci: run pytest on Python 3.13

Adds 3.13 to the pytest matrix in `.github/workflows/pytest.yml` and
drops 3.10 (EOL April 2026).
```

```
chore: regenerate CHANGELOG

`release-please` PR for v1.4.0.
```

```
revert: feat(send): support --resume for interrupted snapshots

This reverts commit abc1234. Resume mode broke replication when the
remote dataset already had a partial snapshot from a previous run;
see #321 for the failure mode. Will re-attempt with a different
strategy.
```

```
feat(cli)!: rename --hosts to --targets

The CLI accepted both --hosts and --targets for the destination
list; --targets matches the documentation and the option name in
the underlying executor module. Drop --hosts.

BREAKING CHANGE: --hosts has been renamed to --targets. Wrapper
scripts and crontabs that pass --hosts must be updated.
```

### Out of scope for this file

CI enforcement of these conventions is tracked separately. This file teaches; the
linter (when configured) enforces.

[First Timers Only]: https://www.firsttimersonly.com/
[good first issues]: https://github.com/alunduil/zfs-replicate/issues?q=is%3Aissue+label%3A%22good+first+issue%22+is%3Aopen
[How to Contribute to an Open Source Project on GitHub]: https://app.egghead.io/playlists/how-to-contribute-to-an-open-source-project-on-github
[Make a Pull Request]: https://makeapullrequest.com/
