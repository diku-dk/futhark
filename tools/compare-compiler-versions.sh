#!/usr/bin/env bash
#
# Quick hack to compare compiler revisions by automatically pulling
# statistics from futhark-lang.org/benchmark-results.
#
# Pass the reference version as the first argument.  You must run this
# while standing in the Futhark directory.

if [ $# -ne 4 ]; then
    echo "Usage: $0 <machine_a> <rev_a> <machine_b> <rev_b>" >&2
    exit 1
fi

RESULTS_URL=https://futhark-lang.org/benchmark-results/
a_url=$RESULTS_URL/$1-$2.json
b_url=$RESULTS_URL/$3-$4.json
echo "Fetching $a_url"
echo "     and $b_url"

tools/cmp-bench-json.py <(curl $a_url) <(curl $b_url)
