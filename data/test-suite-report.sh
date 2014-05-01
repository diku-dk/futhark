#!/bin/sh
#
# Run some basic diagnostics on the test suite.

set -e # Die on error.

echo "The following tests have an .in-file, but no .out-file:"

find data/tests data/benchmarks -type f -name '*fut' | while read -r test; do
    infile=$(echo $test | sed s/.fut$/.in/)
    outfile=$(echo $test | sed s/.fut$/.out/)

    if [ -f "$infile" ] && ! [ -f "$outfile" ]; then
        echo $test
    fi
done
