#!/bin/sh
set -u

FUTHARK_BIN=futhark
TEST_DIR=.

run_test() {
    test_file="$1"
    futhark_cmd="$2"
    # Fixed seed for reproducibility.
    seed_option="--seed=123"


    test_file_full_path="$TEST_DIR/${test_file#./}"

    expected="${test_file%.fut}.out"
    actual="${test_file%.fut}.out_actual"

    ($futhark_cmd test $seed_option "$test_file_full_path") 2>&1 | \
        grep -vE "Compiling with|Running compiled|Running .*/|^$TEST_DIR/|[0-9]+ failed|[0-9]+ passed|[0-9]+/[0-9]+ passed" | \
        sed -E -e 's/seed=-?[0-9]+/seed=REDACTED/g' \
               -e 's/after [0-9]+ tests/after N tests/g' \
               -e 's/candidate=-?[0-9]+/candidate=REDACTED/g' \
               -e 's/random=-?[0-9]+/random=REDACTED/g' \
        > "$actual"

    if diff -u "$expected" "$actual" > /dev/null; then
        echo "✅ PASS: $test_file_full_path"
        rm -f "$actual"
        return 0
    else
        echo "❌ FAIL: $test_file_full_path"
        echo "--- Difference (-expected, +actual) ---"
        diff -u "$expected" "$actual"
        return 1
    fi
}

if [ "${1:-}" = "--worker" ]; then
    run_test "$3" "$2"
    exit $?
fi

echo "Starting Property Tests..."

# Get libraries if necessary.
if [ futhark.pkg -nt lib ]; then
    rm -rf lib
    futhark pkg sync
fi

# 1. Find all .fut files
ALL_TESTS=$(find "." -type d \( -name "lib" -o -name "libraries" \) -prune -o -name "*.fut" -print)

# 2. Filter to only those with a matching .out file
TESTS=$(echo "$ALL_TESTS" | while read -r test; do
    if [ -f "${test%.fut}.out" ]; then
        echo "$test"
    fi
done)

# 3. Get the accurate count of tests that will ACTUALLY run
NUMTESTS=$(echo "$TESTS" | grep -c .)

if [ -z "$TESTS" ]; then
    echo "No tests found."
    exit 0
fi

CORES=$(nproc||echo 4)
echo "$TESTS" | xargs -I {} -P "$CORES" sh "$0" "--worker" "$FUTHARK_BIN" "{}"

# Final status check
if [ $? -ne 0 ]; then
    printf "\nSome tests failed out of the $NUMTESTS tests.\n"
    exit 1
else
    printf "\nAll $NUMTESTS tests passed!\n"
    exit 0
fi
