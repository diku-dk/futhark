-- This test program demonstrates how to simply generate pseudorandom
-- numbers in Futhark.  This is useful for deterministically
-- generating large amounts of test data for a program without
-- actually passing in large arrays from outside.  Note that the
-- quality of the random numbers is very poor, but it's fast to
-- execute and the code is simple.
--
-- ==
-- input { 1i64 -50 50 }
-- output { [26] }
--
-- input { 10i64 -50 50 }
-- output { [10, 38, 31, 12, 12, 0, 0, 23, -15, 37] }
--
-- input { 10i64 0 1 }
-- output { [0, 0, 0, 0, 1, 1, 0, 1, 0, 0] }

-- From http://stackoverflow.com/a/12996028
def hash(x: i32): i32 =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x

def rand_array (n: i64) (lower: i32) (upper: i32): [n]i32 =
  map (\(i: i64): i32  ->
        -- We hash i+n to ensure that a random length-n array is not a
        -- prefix of a random length-(n+m) array.
        hash(i32.i64 (i + n)) % (upper-lower+1) + lower) (
      iota(n))

def main (x: i64) (lower: i32) (upper: i32): []i32 =
  rand_array x lower upper
