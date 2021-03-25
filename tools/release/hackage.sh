#!/bin/sh
#
# Generate and upload documentation to Hackage.

set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -rf "$dir"' EXIT

# assumes cabal 2.4 or later
cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

cabal upload -d --publish $dir/*-docs.tar.gz
