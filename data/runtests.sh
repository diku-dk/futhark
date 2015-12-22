#!/bin/sh
#
# Simple test runner.  Replace me with something sane some day.

# Directory containing tests.
testdir=$(dirname "$0")

compiler=futhark-c
exclude=""

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
        --compiler=*)
            compiler=$(echo $1|cut -d= -f2-)
            shift
            ;;
        --compiler)
            compiler=$2
            shift
            shift
            ;;
        --exclude=*)
            exclude="$exclude --exclude $(echo $1|cut -d= -f2-)"
            shift
            ;;
        --exclude)
            exclude="$exclude --exclude $2"
            shift
            shift
            ;;
        -*) echo "Unknown option $1."; exit 1;;
        --) break ;;
        *) break;;
    esac
done

echo "compiler: $compiler"

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
    futhark_test_command() {
        futhark-test -t --compiler="$compiler" "$@"
    }
elif [ "$onlycompile" = true ]; then
    futhark_test_command() {
        futhark-test --compiler="$compiler" -c "$@"
    }
elif [ "$onlyinterpret" = true ]; then
    futhark_test_command() {
        futhark-test -i "$@"
    }
else
    futhark_test_command() {
        futhark-test  --compiler="$compiler" "$@"
    }
fi

futhark_test_command $exclude $tests
