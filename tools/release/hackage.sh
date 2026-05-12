#!/bin/sh
#
# Generate sdists and documentation for Hackage, then upload them.

set -e

user=TroelsHenriksen
pass=$HACKAGE_KEY

dir=$(mktemp -d dist.XXXXXX)
trap 'rm -rf "$dir"' EXIT

echo "Generating sdist..."
cabal sdist --builddir="$dir"

echo "Uploading sdist..."
cabal upload --publish --username=$user --password=$pass $dir/sdist/*.tar.gz

# At some point Hackage could not generate our documentation, but that
# is currently not the case.
if false; then
    # See https://github.com/haskell/cabal/issues/8104 for why we have --haddock-options=--quickjump
    echo "Generating Haddock..."
    cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc --haddock-options=--quickjump

    echo "Uploading Haddock..."
    cabal upload --publish --username=$user --password=$pass -d $dir/*-docs.tar.gz
fi
