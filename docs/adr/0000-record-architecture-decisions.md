# 0. Record architecture decisions

## Status

Proposed

## Context

An architecturally significant decision -- one that sets precedent,
locks in a dependency, shapes an interface, or picks between non-trivial
options -- loses its rationale after it lands as code. A diff shows what
changed. It doesn't record which options lost out, or why. Months later
a reader rebuilds that reasoning from memory or argues it again from
scratch.

## Decision

This repository keeps Architecture Decision Records under `docs/adr/`,
one file per decision, named `NNNN-kebab-title.md` with a zero-padded
sequence. A record earns its place when a decision both sets direction
and resists reversal. Tactical, reversible choices stay in commit
messages and pull request descriptions.

This meta-record uses the short
[Nygard](https://www.cognitect.com/blog/2011/11/15/documenting-architecture-decisions)
form. A record whose weight lies in comparing options uses
[MADR](https://adr.github.io/madr/) instead, so the considered options
and their trade-offs stand as their own sections rather than dissolving
into prose.

Every record starts at `Proposed`. It moves to `Accepted` after the team
commits to the decision, or to `Superseded by NNNN` or `Deprecated` when
a later record replaces it.

## Consequences

The rationale behind a hard choice survives next to the code it governs,
and a new contributor reads the collection in sequence to see how the
current design took shape. The cost is discipline: writing a record when
a decision warrants one, and the judgment to skip the rest. A collection
padded with tactical notes is worth less than a spare one.
