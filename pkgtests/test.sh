#!/bin/sh
#
# You must be in the directory when running this script.  It does not
# try to be clever.

set -e # Die on error.

lastrun=""

expects () {
    # Hack to correctly handle empty directories, which are otherwise
    # not committed to Git.
    if ! diff -urN $1 $2; then
        echo "After command '$lastrun', $1 does not match $2"
        exit 1
    fi
}

i=0

succeed () {
    lastrun="$@"
    echo '$' "$@"
    if ! "$@"; then
        echo "Command '$lastrun' failed unexpectedly."
        exit 1
    fi
    expects futhark.pkg futhark.pkg.$i
    expects lib lib.$i
    i=$(($i+1))
}

fail () {
    lastrun="$@"
    echo '$' "$@"
    if "$@"; then
        echo "Command '$lastrun' succeeded unexpectedly."
        exit 1
    fi
}

# Clean up after previous test runs.
rm -rf futhark.pkg lib

succeed futhark-pkg create github.com/sturluson/testpkg

succeed futhark-pkg add github.com/athas/fut-foo 0.1.0

succeed futhark-pkg sync

succeed futhark-pkg add github.com/athas/fut-baz 0.1.0

succeed futhark-pkg sync

succeed futhark-pkg upgrade

succeed futhark-pkg sync

succeed futhark-pkg remove github.com/athas/fut-foo

succeed futhark-pkg sync

succeed futhark-pkg add github.com/athas/fut-foo@2 2.0.0

succeed futhark-pkg sync
