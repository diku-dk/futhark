#!/bin/sh
#
# Generate adists and documentation for Hackage.  Only creates package
# candidates, which are then manually enabled.

set -e

user=TroelsHenriksen
pass=$HACKAGE_KEY

dir=$(mktemp -d dist.XXXXXX)
trap 'rm -rf "$dir"' EXIT

echo "Generating sdist..."
cabal sdist --builddir="$dir"

echo "Uploading sdist..."
cabal upload --username=$user --password=$pass $dir/sdist/*.tar.gz

echo "Generating Haddock..."
cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

echo "Uploading Haddock..."
cabal upload --username=$user --password=$pass -d $dir/*-docs.tar.gz
