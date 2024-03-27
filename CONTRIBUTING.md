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

See [Code of Conduct](./CODE_OF_CONDUCT.md) for more.

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

[First Timers Only]: https://www.firsttimersonly.com/
[good first issues]: https://github.com/alunduil/zfs-replicate/issues?q=is%3Aissue+label%3A%22good+first+issue%22+is%3Aopen
[How to Contribute to an Open Source Project on GitHub]: https://app.egghead.io/playlists/how-to-contribute-to-an-open-source-project-on-github
[Make a Pull Request]: https://makeapullrequest.com/
