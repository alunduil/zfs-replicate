# Commit message conventions (AI agents)

Full conventions live in [`CONTRIBUTING.md`](../../CONTRIBUTING.md#commit-messages) -
that document is the single source of truth for humans and bots alike. Read it first.

## Shorthand for AI agents

When generating a commit message, produce this shape:

```
<type>[optional scope][!]: <imperative, <=50 chars, no trailing period>

<body wrapped at 72 chars, explaining what and why>
```

- `<type>` is one of: `feat`, `fix`, `perf`, `refactor`, `docs`, `test`, `build`, `ci`,
  `chore`, `revert`. Definitions and `release-please` bumps are in
  [CONTRIBUTING.md - Allowed types](../../CONTRIBUTING.md#allowed-types).
- Scope is optional. Active scopes: `send`, `cli`, `executor`, `ci`, `docs`, `deps`.
  See [CONTRIBUTING.md - Scope policy](../../CONTRIBUTING.md#scope-policy).
- Breaking change: add `!` before the colon **and** a `BREAKING CHANGE:` footer. See
  [CONTRIBUTING.md - Breaking changes](../../CONTRIBUTING.md#breaking-changes).

For type definitions, scope guidance, and the breaking-change format, follow the
CONTRIBUTING.md links above; don't duplicate them here.
