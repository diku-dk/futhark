#!/bin/sh
#
# Simple test runner.  Replace me with something sane some day.

# Directory containing tests.
testdir=$(dirname "$0")

# Command to run the L0 compiler.
#l0c="l0c -frute"
l0c="l0c -ei"

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

for test in $tests; do
    infile=$(echo "$test" | sed 's/l0$/in/')
    outfile=$(echo "$test" | sed 's/l0$/out/')
    if ! [ -f "$test" ]; then
        echo "$test not found."
        exit 1
    fi
    if [ -f "$infile" ]; then
        # There is an .in-file, so we assume the test program must be correct.
        if [ -f "$outfile" -a ! "$onlytypecheck" ]; then
            # There is even an .out-file, so it must have some expected output.
            testoutfile=$(echo "$test" | sed 's/l0$/testout/')
            echo "Testing $test (expecting correct execution)."
            if cat "$infile" | $l0c -iu "$test" > "$testoutfile"; then
                if ! cmp -s "$outfile" "$testoutfile"; then
                    echo "$testoutfile and $outfile do not match."
                fi
            fi
        else
            # No output file, so just check whether it is type-correct.
            echo "Testing $test (expecting type-correctness)."
            $l0c "$test" < "$infile" > /dev/null
        fi
    else
        # No .in-file, so this is a negative test that must result in
        # a compile error.
        echo "Testing $test (expecting type failure)."
        if $l0c "$test" > /dev/null 2>&1; then
            echo Compiled, but should have failed.
        fi
    fi
done
