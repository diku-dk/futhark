#!/bin/sh
#
# Test all the test programs, excluding the wip directory, with reuse
# enabled.

cd "$(dirname "$0")"

dirs_and_files() {
    ls *.fut 2>/dev/null
    find -type d | grep -Ev -e '^\.$' -e '^\./wip$' -e '^\./wip/'
}

export MEMORY_BLOCK_MERGING_REUSE=1
futhark-test --compiler=futhark-c $(dirs_and_files)
