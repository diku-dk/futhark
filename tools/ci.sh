#!/bin/sh
#
# The Futhark continuous integration tool.  Enjoy.
#
# Hardcoded to only send email when running as 'concieggs' user.

set -e # Die on error.

memlimit=$((1024 * 512)) # In KiB

maxtesttime=$((60 * 20)) # The length of time the test suite is
                         # permitted to run for (in seconds).

mail=athas@sigkill.dk # Where to send error reports.
frommail=concieggs@eggsml.dk # The sender of said reports.

docdir=/var/www/futhark.sigkill.dk

if ! [ $# = 1 ]; then
    echo "Usage: $0 <futharkdir>"
    exit 1
fi

futharkdir=$1
lockfile="$futharkdir/ci.lock"
outfile=$(mktemp) || exit 1 # Hm, silent failure...

ulimit -m $memlimit

isBot () {
    [ $(whoami) = concieggs ]
}


cmd() {
    echo "% $@"
    $@ 2>&1
}

update() {
    cmd git pull
}

build() {
    cmd cabal install --only-dependencies --enable-tests &&
    cmd cabal clean &&
    cmd cabal configure --enable-tests &&
    cmd cabal build
}

runtests() {
    PATH=dist/build/futhark/:$PATH cmd timeout $maxtesttime data/runtests.sh
    status=$?
    if [ $status = 124 ]; then
        echo "Test suite exceeded permitted run time"
    fi
    return $status
}

cabaltests() {
    cmd cabal test
    status=$?
    if [ $status = 124 ]; then
        echo "Cabal test suite exceeded permitted run time"
    fi
    return $status
}

ci() {
    (
        flock -n 9 || exit 1
        # We now have an exclusive lock!
        cd "$futharkdir" &&
        update &&
        build &&
        runtests &&
        cabaltests
    ) 9>$lockfile >>$outfile 2>&1
}

mail() {
    if isBot; then
        mailx -s "Futhark integration error" "$mail" -- -r $frommail
    else
        echo "Build failed, but since I am not concieggs, I will not send any email."
        echo "The contents of the email would have been as follows."
        echo
        cat
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
else
    if isBot; then
        (
            cd "$futharkdir" && \
                cabal haddock --html --hyperlink-source && \
                cp -r dist/doc/html/futhark/* $docdir
        )
    else
        echo "Not concieggs, so not building documentation."
    fi
fi

rm $outfile
