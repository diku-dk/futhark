#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset

cabal update
cabal install --install-method=copy --overwrite-policy=always

PATH=~/.cabal/bin:$PATH futhark test --no-terminal tests examples --no-tuning
PATH=~/.cabal/bin:$PATH make -C libtests/c
