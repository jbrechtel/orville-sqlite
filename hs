#!/usr/bin/env bash
# Thin wrapper to run Haskell tooling (stack, hpack, fourmolu, hlint, ...)
# inside a Docker image with our system dependencies pre-installed.
# Usage: ./hs <cmd> [args]
set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Build a custom image on top of flipstone/haskell-tools that includes
# the C libraries our Haskell packages need (e.g. libsqlite3-dev).
IMAGE="orville-sqlite-haskell-tools"

docker build \
  --quiet \
  -t "${IMAGE}" \
  -f - \
  "${PROJECT_DIR}" << DOCKERFILE
FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-5d6640d
RUN apt-get update -qq && apt-get install -y -qq libsqlite3-dev
DOCKERFILE

# Named Docker volume shared across all worktrees so cached
# GHC/dependencies don't need rebuilding per worktree.
STACK_ROOT_VOLUME="orville-sqlite-stack-root"

docker volume inspect "${STACK_ROOT_VOLUME}" > /dev/null 2>&1 || \
  docker volume create "${STACK_ROOT_VOLUME}" > /dev/null

exec docker run --rm -i $([ -t 0 ] && printf -- -t) \
  -v "${PROJECT_DIR}:/work" \
  -v "${STACK_ROOT_VOLUME}:/stack-root" \
  -e STACK_ROOT=/stack-root \
  -w /work/orville-sqlite \
  "${IMAGE}" \
  "$@"
