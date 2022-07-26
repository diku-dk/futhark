#!/bin/sh
#
# Automatically apply hlint rules to the given file(s) and directories.
#
# Note: you may need to run this tool to a fixed point manually, as
# some rewrites only do partial improvements, and the resulting code
# may still violate other rules.

for d in "$@"; do
    find "$d" -name \*.hs | parallel hlint --refactor --refactor-options=-i
done
