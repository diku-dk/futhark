#!/bin/sh
#
# Simple test runner.  Replace me with something sane some day.

# Directory containing tests.
testdir=$(dirname "$0")

# Command to run the L0 compiler.
l0c="l0c -ft"

tests() {
    echo $testdir/*l0
#    echo $testdir/uniqueness*l0
#    echo $testdir/PricingLexiFi.l0
#    echo $testdir/HiperfitExample1.l0
#    echo $testdir/CalibLexiFi.l0
}

for test in $(tests); do
    infile=$(echo "$test" | sed 's/l0$/in/')
    outfile=$(echo "$test" | sed 's/l0$/out/')
    if [ -f "$infile" ]; then
        # There is an .in-file, so we assume the test program must be correct.
        if [ -f "$outfile" ]; then
            # There is even an .out-file, so it must have some expected output.
            echo "Testing $test (expecting execution)."
            echo "cannot test output yet."
        else
            # No output file, so just check whether it is type-correct.
            echo "Testing $test (expecting type-correctness)."
            $l0c "$test" < "$infile" > /dev/null;
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
