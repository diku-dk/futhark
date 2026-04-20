#!/bin/sh
set -u

FUTHARK_BIN="${1:-cabal run -v0 -- futhark}"
TEST_DIR="tests_property"

run_test() {
    test_file="$1"
    futhark_cmd="$2"
    seed_flag=""

    base_name=$(basename "$test_file")

    # Special cases where output varies due to randomness.
    # Use specific seeds for these tests to ensure deterministic output.
    case "$base_name" in
        "i8AutoShrink.fut" | "i32NoShrink.fut")
            seed_flag="--seed=-1161324613"
            ;;
        "f64.fut")
            seed_flag="--seed=-540792207"
            ;;
        Record* | Tuple* | "Stepping.fut")
            seed_flag="--seed=-2070821161"
            ;;
    esac

    test_file_full_path="$TEST_DIR/${test_file#./}"

    expected="${test_file%.fut}.out"
    actual="${test_file%.fut}.outactual"

    (cd .. && $futhark_cmd test $seed_flag "$test_file_full_path") 2>&1 | \
        grep -vE "Compiling with|Running compiled|Running .*/|^$TEST_DIR/|[0-9]+ failed|[0-9]+ passed|[0-9]+/[0-9]+ passed" | \
        sed -E -e 's/seed=-?[0-9]+/seed=REDACTED/g' \
               -e 's/after [0-9]+ tests/after N tests/g' \
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
    echo "\nSome tests failed out of the $NUMTESTS tests."
    exit 1
else
    echo "\nAll $NUMTESTS tests passed!"
    exit 0
fi
