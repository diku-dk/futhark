#!/bin/sh
#
# Like oclgrindrunner, but also uses Python.

set -e

log=$(mktemp oclgrindgrunner-XXXXXX)
trap 'rm -f "$log"' EXIT

# We only use one worker thread, because we are probably going to be
# running this as part of the test suite, where parallelism comes from
# the large number of test programs.

oclgrind --max-errors 10 --log "$log" --num-threads 1 python3 "$@"

if [ "$(stat -c%s "$log")" -ne 0 ]; then
    cat "$log" >&2
    rm -f "$log"
    exit 1
else
    rm -f "$log"
fi
