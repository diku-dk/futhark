#!/bin/sh
#
# Test that 'futhark eval' produces the same result whether we use the
# interpreter for everything or compile the entry points.

FILE=test.fut
BACKEND=c

failed=0

interpreted() {
    # Assumption: 'futhark eval' defaults to interpreted code unless --backend
    # is passed.
    futhark eval -w -f $FILE "$1" 2>&1
}

external() {
    # We use --skip-compilation so we have to compile up front manually.
    futhark eval -w --backend=$BACKEND --skip-compilation -f $FILE "$1" 2>&1
}

check() {
    expected=$(interpreted "$1"); expected_status=$?
    actual=$(external "$1"); actual_status=$?

    if [ "$actual" = "$expected" ] && [ $actual_status = $expected_status ]; then
        echo "PASS: $1"
    else
        echo "FAIL: $1"
        echo "  interpreted (exit $expected_status): $expected"
        echo "  external (exit $actual_status): $actual"
        failed=1
    fi
}

# Guard against the tests passing vacuously: if the entry points were
# not actually being run through the server, then evaluation would
# succeed even with no compiled program present.
rm -f test
if external p1 > /dev/null 2>&1; then
    echo "FAIL: evaluation succeeded without a compiled program."
    exit 1
fi

futhark $BACKEND --server $FILE || exit 1

# Preloaded primitives
check p1
check p2
check p3

# Preloaded records
check r1
check r2
check r3

# Preloaded sums
check s1
check s2
check s3

# Preloaded primitive arrays
check pa1
check 'pa1[0]'
check 'pa1[1]'
check 'pa1[2]'
check 'pa2[1,0]'
check 'pa2[1,1]'
check 'pa2[1,2]'
check 'pa3[2,0]'
check 'pa3[2,1]'

# Out-of-bounds indexing must be caught by the interpreter and give the right
# error message.
check 'pa1[3]'
check 'pa1[-1]'
check 'pa2[2,0]'
check 'pa3[0,2]'

# Slicing, including slices of an already-indexed array.
check 'pa1[0:3]'
check 'pa1[1:]'
check 'pa2[0]'
check 'pa2[1,0:2]'
check 'pa2[1,0:3]'
check 'pa2[1,0:0]'
check 'pa3[0:2,1]'
check 'pa3[1,0:2]'

# The shape of a slice of an already-indexed array is observable in the error
# message produced by indexing it out of bounds.
check '(pa2[1,0:2])[5]'
check '(pa3[1,0:2])[5]'

# Consuming an array with something other than indexing.
check 'length pa1'
check 'map (+1) pa1'
check 'i32.sum pa1'
check 'reverse pa1'
check 'map i32.sum pa2'
check 'zip pa1 pa1'
check 'pa1 with [0] = 5'
check 'copy pa2 with [1,2] = 5'

# Preloaded record arrays
check ra1
check 'ra1[0].x'
check 'ra1[1].x'
check 'ra1[2].x'
check 'ra2[1,0].y'
check 'ra2[1,1].y'
check 'ra2[1,2].y'
check 'map (.x) ra1'

# Preloaded sum arrays
check sa1
check 'sa1[0]'
check 'sa1[1]'
check 'sa1[2]'
check 'sa2[1,0]'
check 'sa2[1,1]'
check 'sa2[1,2]'

# Primitive functions
check 'pf 2'
check 'pf 3'

# Record functions
check 'rf {x = 1, y = 2}'
check 'rf {x = 2, y = 1}'

# Sum functions
check 'sf (#a 2)'
check 'sf (#b 2)'

# Primitive array functions
check 'pa1f [1,2,3]'
check '(pa1f [1,2,3])[0]'
check '(pa1f [1,2,3])[1]'
check '(pa1f [1,2,3])[2]'
check 'pa2f [[1,2,3], [3,2,1]]'
check '(pa2f [[1,2,3], [3,2,1]])[0,0]'
check '(pa2f [[1,2,3], [3,2,1]])[0,1]'
check '(pa2f [[1,2,3], [3,2,1]])[0,2]'

# An array produced by one external function, passed to another.
check 'pa1f pa1'
check 'pa2f pa2'

# Record array functions
check 'ra1f ra1'
check 'ra2f ra2'
check '(ra1f ra1)[0].x'

# Sum array functions
check 'sa1f sa1'
check 'sa2f sa2'
check '(sa1f sa1)[0]'

exit $failed
