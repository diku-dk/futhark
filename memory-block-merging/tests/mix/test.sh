#!/bin/sh
#
# Test all the test programs, excluding the wip directory, with both coalescing
# and reuse enabled.
#
# With -w, exclude the wip directory.

cd "$(dirname "$0")"

if [ "$1" = '-w' ]; then
dirs_and_files() {
    ls *.fut 2>/dev/null
    find -type d | grep -Ev -e '^\.$' -e '^\./wip$' -e '^\./wip/'
}
else
    dirs_and_files() {
        echo .
    }
fi

export MEMORY_BLOCK_MERGING_COALESCING=1
export MEMORY_BLOCK_MERGING_REUSE=1
futhark-test --compiler=futhark-c $(dirs_and_files)
