#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset

tools/style-check.sh src || echo "::error:: A style violation was found" && exit 1

# cabal installs here.
export PATH=$HOME/.cabal/bin:$PATH

cabal update

cabal test || echo "::error:: Unit tests failed" && exit 1

# When running in a pure nix-shell, we need to tell git where it can
# find certificates.
export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt

cabal install --install-method=copy --overwrite-policy=always --installdir=$HOME/.cabal/bin || echo "::error:: Could not build futhark" && exit 1

echo "# Integration tests"
futhark test --no-terminal tests examples --no-tuning || echo "::error:: Integration tests failed" && exit 1

echo "# Library tests"
make -C libtests/c || echo "::error:: Library tests failed" && exit 1

echo "# Package tests"
(cd pkgtests; sh test.sh) || echo "::error:: Package tests failed" && exit 1
