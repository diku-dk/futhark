#!/bin/sh
#
# This script can be used as the --runner argument to the 'bench' and
# 'test' subcommands.  It will use oclgrind to run the program, and
# return a non-zero exit code if oclgrind produces any error output.

set -e

log=$(mktemp oclgrindgrunner-XXXXXX)
trap 'rm -f "$log"' EXIT

# We only use one worker thread, because we are probably going to be
# running this as part of the test suite, where parallelism comes from
# the large number of test programs.

oclgrind --max-errors 10 --log "$log" --num-threads 1 "$@"

if [ "$(stat -c%s "$log")" -ne 0 ]; then
    cat "$log" >&2
    rm -f "$log"
    exit 1
else
    rm -f "$log"
fi
