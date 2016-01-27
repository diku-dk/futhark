-- This test program demonstrates how to simply generate pseudorandom
-- numbers in Futhark.  This is useful for deterministically
-- generating large amounts of test data for a program without
-- actually passing in large arrays from outside.  Note that the
-- quality of the random numbers is very poor, but it's fast to
-- execute and the code is simple.
--
-- ==
-- input { 1 -50 50 }
-- output { [26] }
--
-- input { 10 -50 50 }
-- output { [10, 38, 31, 12, 12, 0, 0, 23, -15, 37] }
--
-- input { 10 0 1 }
-- output { [0, 0, 0, 0, 1, 1, 0, 1, 0, 0] }

-- From http://stackoverflow.com/a/12996028
fun int hash(int x) =
  let x = ((x >> 16) ^ x) * 0x45d9f3b in
  let x = ((x >> 16) ^ x) * 0x45d9f3b in
  let x = ((x >> 16) ^ x) in
  x

fun [int,n] rand_array(int n, int lower, int upper) =
  let max_rounds = 5 in
  map(fn int (int i) =>
        -- We hash i+n to ensure that a random length-n array is not a
        -- prefix of a random length-(n+m) array.
        hash(i+n) % (upper-lower+1) + lower,
      iota(n))

fun [int] main(int x, int lower, int upper) =
  rand_array(x, lower, upper)
