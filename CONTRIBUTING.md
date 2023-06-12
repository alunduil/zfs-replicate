# Contributing

## Introduction

Thanks for considering contributing to zfs-replicate.  Contributors are
instrumental for ensuring zfs-replicate remains easy to use and functioning
correctly.

Following these guidelines ensures everyone is working with consistent
expectations.  Repository owners and contributors are busy people and we want to
ensure respectful interactions that push this project to improve.

We're open to pretty much any contributions you want to submit as a pull request
or issue to this repository.  We don't have any other communication channels at
this time so feel free to use issues to report problems or ask for assistance.

## Ground Rules

* Pull requests require an owner's review
* Pull requests require status checks to be passing
* Expect discussion on your pull requests so more than just you understand the
  changes you're making
* Remember to explain why you want your issue or pull request to be included

See our [Code of Conduct](./CODE_OF_CONDUCT.md) for more.

## Your First Contribution

If you're unsure of what to contribute, check out our list of "[good first
issues]."

If you're completely new to GitHub and Open Source, check out:

* [How to Contribute to an Open Source Project on GitHub]
* [Make a Pull Request]
* [First Timers Only]

If you have any questions, feel free to ask in your pull requests or open an
issue.

## Getting Started

1. Create your own fork of zfs-replicate
1. If not using VS Code, run `poetry install`
1. If not using VS Code, run `poetry shell`
1. Run `pre-commit install` in your local checkout
1. Make the changes in your fork
1. Test your changes with `pytest`
1. Send your changes in a new pull request describing why you want to make that
   change

For small changes, feel free to use the in GitHub editor or skip running the
tests locally (they'll be double checked in the pull request automatically).

Small change examples:

* Spelling or grammar fixes
* Typo corrections, white space, and formatting changes
* Comment clean ups

## How to Report a Bug

When creating an issue for a bug or unexpected behaviour, make sure you include
the following information:

1. A link to a paste with your `poetry.lock` file if available
1. A link to a paste with your `/etc/os-release` file (or equivalent for Mac or
   Windows)
1. What command did you run (please remove passwords or secrets if present)
1. What happened?
1. What did you expect to happen?

## How to Suggest a Feature or Enhancement

zfs-replicate aims to be a simple to use wrapper around the ssh and zfs command
line tools.  Other tools exist that fill more complicated use cases.

To request a new feature or ask if a feature fits the above philosophy, open an
issue describing the behaviour you desire and why it would be useful to you or
others.  If you can, include specific examples of command invocations and side
effects to ensure we understand the request correctly.

## Code Review Process

The owners regularly review pull requests (_if a review is pinged by the stale
issue action, feel free to comment asking for an ETA_).  Once an owner has
accepted a pull request they will merge it if all status checks are passing.  If
you have an interest in being an owner of this project, simply ask.

## Community

* [Current Contributors](https://github.com/alunduil/zfs-replicate/graphs/contributors)
* Responses to issues and pull requests should be expected within two weeks of submission.

[First Timers Only]: https://www.firsttimersonly.com/
[good first issues]: https://github.com/alunduil/zfs-replicate/issues?q=is%3Aissue+label%3A%22good+first+issue%22+is%3Aopen
[How to Contribute to an Open Source Project on GitHub]: https://app.egghead.io/playlists/how-to-contribute-to-an-open-source-project-on-github
[Make a Pull Request]: https://makeapullrequest.com/
