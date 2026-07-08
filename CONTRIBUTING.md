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
1. Test your changes with `pytest`, following the
   [testing conventions](docs/reference/testing.md)
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
the CI commit-lint check passes the first time, `release-please` places each commit in
the right section of the CHANGELOG, and the correct semver bump comes out the other end.
Subjects are imperative, capped near 50 characters, no trailing period; bodies wrap at
72 and explain _what_ and _why_. See [Tim Pope's note on git commit messages](https://cbea.ms/git-commit/)
for the underlying rules.

### Subject format

```
<type>[optional scope][!]: <description>
```

A breaking-change marker (`!`) immediately before the colon causes `release-please` to
issue a major-version bump regardless of `<type>`.

### Allowed types

| Type       | When to use it                                                     | release-please bump | Example                                                              |
| ---------- | ------------------------------------------------------------------ | ------------------- | -------------------------------------------------------------------- |
| `feat`     | New user-visible capability                                        | minor               | `feat(send): support --resume for interrupted snapshots`             |
| `fix`      | Bug fix                                                            | patch               | `fix(executor): close ssh transport on early exit`                   |
| `perf`     | Performance improvement with no functional change                  | patch               | `perf(send): pipeline mbuffer between local and remote zfs`          |
| `refactor` | Internal change with no functional or performance impact           | none                | `refactor(cli): extract argument parsing into its own module`        |
| `docs`     | Documentation only                                                 | none                | `docs: clarify --recursive flag in README`                           |
| `test`     | Adding or fixing tests                                             | none                | `test(executor): cover the timeout-vs-cancel branch`                 |
| `build`    | Build system, packaging, or dependency updates                     | none                | `build(deps): bump cryptography to 44.0.1`                           |
| `ci`       | CI configuration                                                   | none                | `ci: run pytest on Python 3.13`                                      |
| `chore`    | Maintenance with no source impact                                  | none                | `chore: regenerate CHANGELOG`                                        |
| `revert`   | Revert of a previous commit                                        | matches reverted    | `revert: feat(send): support --resume for interrupted snapshots`     |

A commit that introduces a breaking change uses the type that best describes the change
and adds the `!` marker, regardless of which bump that type would normally produce.

### Scope policy

Scope is **optional**. When the change touches a single module, use the module name in
parentheses. The scopes currently in active use:

- `send` - `zfs/send.py` and adjacent send-pipeline code
- `cli` - argparse plumbing and entry points
- `executor` - subprocess / SSH wrappers
- `ci` - anything under `.github/workflows/`
- `docs` - README, CONTRIBUTING, this file
- `deps` - paired with `build:` for dependency bumps (`build(deps): ...`)

Add new scopes when they earn it; a one-off edit in a module isn't a new scope.

### Breaking changes

Two ways to mark a breaking change. **Both** must appear; the marker without the footer
slips past code review more often than you'd expect.

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

`docs`, `test`, `refactor`, `build`, `ci`, `chore`, `revert` (without `!`) don't
release on their own.

[First Timers Only]: https://www.firsttimersonly.com/
[good first issues]: https://github.com/alunduil/zfs-replicate/issues?q=is%3Aissue+label%3A%22good+first+issue%22+is%3Aopen
[How to Contribute to an Open Source Project on GitHub]: https://app.egghead.io/playlists/how-to-contribute-to-an-open-source-project-on-github
[Make a Pull Request]: https://makeapullrequest.com/
