#!/bin/sh
#
# Test all the test programs, excluding the wip directory, with reuse enabled.
#
# With -w, exclude the wip directory.

base="$(dirname "$0")"

compiler="$1"
if ! [ "$compiler" ]; then
    compiler='futhark-c'
fi

if [ "$2" = '-w' ]; then
dirs_and_files() {
    ls $base/*.fut 2>/dev/null
    find $base -type d | grep -Ev -e '^\.$' -e '^\./wip$' -e '^\./wip/'
}
else
    dirs_and_files() {
        echo $base
    }
fi

export IN_PLACE_LOWERING=0
export MEMORY_BLOCK_MERGING_REUSE=1
futhark-test --compiler=$compiler $(dirs_and_files)
