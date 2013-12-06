#!/bin/sh
#
# The L0C continuous integration tool.  Enjoy.
#
# Hardcoded to only send email when running as 'concieggs' user.

set -e # Die on error.

memlimit=$((1024 * 512)) # In KiB

maxtesttime=$((60 * 20)) # The length of time the test suite is
                         # permitted to run for (in seconds).

mail=athas@sigkill.dk # Where to send error reports.

if ! [ $# = 1 ]; then
    echo "Usage: $0 <l0dir>"
    exit 1
fi

l0dir=$1
lockfile="$l0dir/ci.lock"
outfile=$(mktemp) || exit 1 # Hm, silent failure...

ulimit -m $memlimit

cmd() {
    echo "% $@"
    $@ 2>&1
}

update() {
    cmd git pull
}

build() {
    cmd cabal install --only-dependencies &&
    cmd cabal clean &&
    cmd cabal configure &&
    cmd cabal build
}

runtests() {
    PATH=dist/build/l0c/:$PATH cmd timeout $maxtesttime data/runtests.sh -t
    status=$?
    if [ $status = 124 ]; then
        echo "Test suite exceeded permitted run time"
    fi
    return $status
}

ci() {
    (
        flock -n 9 || exit 1
        # We now have an exclusive lock!
        cd "$l0dir" &&
        update &&
        build &&
        runtests
    ) 9>$lockfile >>$outfile 2>&1
}

mail() {
    if [ $(whoami) != concieggs ] ; then
        echo "Build failed, but since I am not concieggs, I will not send any email."
        echo "The contents of the email would have been as follows."
        echo
        cat
    else
        mailx -s "L0C integration error" "$mail" -- -r concieggs@eggsml.dk
    fi
}

if ! ci; then
    (
        echo "Test failed at $(date) of commit"
        git log --pretty=format:'%h: %s' -n 1
        echo
        echo "Log follows."
        echo
        cat $outfile
    ) | mail
fi

rm $outfile
