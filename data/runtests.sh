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
        -*) echo "Unknown option $1."; exit 1;;
        --) break ;;
        *) break;;
    esac
done

tests() {
    echo $testdir/*l0
#    echo $testdir/uniqueness*l0
#    echo $testdir/PricingLexiFi.l0
#    echo $testdir/HiperfitExample1.l0
#    echo $testdir/CalibLexiFi.l0
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
else
    $testdir/Runtests.hs $tests
fi
