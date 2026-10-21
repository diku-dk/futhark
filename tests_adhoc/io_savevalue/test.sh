#!/bin/sh
#
# Test that 'io.savevalue' writes the binary data format, and that it is not
# permitted in the untrusted code of a test specification.

set -e

futhark eval 'io.savevalue "saved.bin" (1i32, [2i64,3i64])' > /dev/null
echo '1i32 [2i64,3i64]' | futhark dataset --binary > expected.bin
cmp saved.bin expected.bin

# The element type of an empty array comes from the type of the value, as the
# value itself does not determine it.
futhark eval 'io.savevalue "saved.bin" (filter (>5) [1i32,2,3])' > /dev/null
[ "$(futhark dataset --text < saved.bin)" = 'empty([0]i32)' ]

# Only what the data format can represent may be saved.
! futhark eval 'io.savevalue "saved.bin" (zip [1i32] [2i32])' 2> stderr
grep -q 'only primitives, arrays of primitives, and tuples of these' stderr
! futhark eval 'io.savevalue "saved.bin" {a=1i32}' 2> stderr
grep -q 'only primitives, arrays of primitives, and tuples of these' stderr

# A value residing on a server is written by the server itself, rather than
# fetched, while one that is only partly there is fetched an array at a time.
# Either way the file must be byte for byte what the interpreter writes alone.
for v in 'mkarr 3' '(mkarr 2, mkarr 3)' '(7i32, mkarr 2)' '(mkarr2d 3)[1]'; do
    futhark eval -w -f srv.fut --backend=c "io.savevalue \"saved.bin\" ($v)" > /dev/null
    futhark eval -w -f srv.fut "io.savevalue \"expected.bin\" ($v)" > /dev/null
    cmp saved.bin expected.bin
done

# And nothing at all may be saved by a test specification.
! futhark test unsafe.fut > stderr 2>&1
grep -q 'not permitted in untrusted code' stderr
! [ -f evil.bin ]
