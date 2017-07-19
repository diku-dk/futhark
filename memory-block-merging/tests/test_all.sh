#!/bin/sh
#
# Run all the tests in the subdirectories.
#
# With -a, do not exclude the wip directories.

cd "$(dirname "$0")"

./coalescing/test.sh "$1" \
    && ./reuse/test.sh "$1" \
    && ./mix/test.sh "$1"
