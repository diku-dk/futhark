#!/bin/sh
#
# Run all the tests in the subdirectories.

cd "$(dirname "$0")"

./coalescing/test.sh "$@" \
    && ./reuse/test.sh "$@" \
    && ./mix/test.sh "$@"
