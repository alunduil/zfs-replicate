#!/usr/bin/env bash
# Post-create hook for the dev container. Installs Python deps,
# pre-commit hooks, and Vale (the linter previously provided by the
# unmaintained ghcr.io/shinepukur/devcontainer-features/vale feature).
set -euo pipefail

# 1) Project Python dependencies.
poetry install

# 2) Install the pre-commit hook into the local clone.
pre-commit install

# 3) Vale linter. Pinned to a recent stable; bump when CI starts using
#    a newer rule pack.
VALE_VERSION="3.10.1"
VALE_ARCH="64-bit"
VALE_TARBALL="vale_${VALE_VERSION}_Linux_${VALE_ARCH}.tar.gz"
VALE_URL="https://github.com/errata-ai/vale/releases/download/v${VALE_VERSION}/${VALE_TARBALL}"

if ! command -v vale >/dev/null 2>&1; then
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    curl -fsSL "$VALE_URL" -o "$tmp/$VALE_TARBALL"
    tar -xzf "$tmp/$VALE_TARBALL" -C "$tmp" vale
    sudo mv "$tmp/vale" /usr/local/bin/vale
    sudo chmod +x /usr/local/bin/vale
fi
