#!/bin/sh
#
# Simple test runner.  Replace me with something sane some day.

# Directory containing tests.
testdir=$(dirname "$0")

while true; do
    case $1 in
        -t) echo Only type-checking;
            onlytypecheck=true;
            shift
            ;;
        -c) echo Only type-checking and compiling;
            onlycompile=true;
            shift
            ;;
        -i) echo Only type-checking and interpreting;
            onlyinterpret=true;
            shift
            ;;
        -*) echo "Unknown option $1."; exit 1;;
        --) break ;;
        *) break;;
    esac
done

tests() {
    find $testdir/tests/ -name '*fut'
    find $testdir/benchmarks/ -name '*fut'
}

# You can control exactly which tests to run by passing their names as
# arguments on the command line.
if [ $# -gt 0 ]; then
    tests=$*
else
    tests=$(tests)
fi

if [ "$onlytypecheck" = true ]; then
    $testdir/Runtests.hs -t $tests
elif [ "$onlycompile" = true ]; then
    $testdir/Runtests.hs -c $tests
elif [ "$onlyinterpret" = true ]; then
    $testdir/Runtests.hs -i $tests
else
    $testdir/Runtests.hs $tests
fi
