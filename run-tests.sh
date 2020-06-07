#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset

# cabal installs here.
export PATH=$HOME/.cabal/bin:$PATH

# When running in a pure nix-shell, we need to tell git where it can
# find certificates.
export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt

cabal install --install-method=copy --overwrite-policy=always --installdir=$HOME/.cabal/bin

echo "# Integration tests"
futhark test tests examples --no-tuning

echo "# Library tests"
make -C libtests/c

echo "# Package tests"
(cd pkgtests; sh test.sh)
