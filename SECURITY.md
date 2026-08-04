# Security policy

## Supported versions

`zfs-replicate` follows [semantic versioning](https://semver.org/). Security
fixes land on the most recent release in the current major series. Earlier
major series receive no fixes.

| Version | Supported |
| ------- | --------- |
| 4.x     | Yes       |
| < 4.0   | No        |

## Reporting a vulnerability

Report security vulnerabilities privately rather than opening a public issue.

- Preferred: use GitHub's [private vulnerability reporting][advisory], the
  "Report a vulnerability" button on the repository's Security tab. Reports stay
  private to the maintainer until I publish an advisory.
- Alternative: email Alex Brandt at <alunduil@gmail.com>.

Include the affected version, a description of the issue, and, if you can, the
steps or input needed to reproduce it. `zfs-replicate` invokes `ssh` and `zfs`
with user-supplied host names and data set names, so reproduction details for
that surface help the most.

## What to expect

This is a single-maintainer project, so responses are best-effort. Expect an
acknowledgment within a couple of weeks. Once I confirm a report, I ship a fix
and publish an advisory as soon as is practical.

## Credit

I credit reporters in the advisory and release notes by default. Say so in your
report if you would rather remain anonymous.

[advisory]: https://github.com/alunduil/zfs-replicate/security/advisories/new
