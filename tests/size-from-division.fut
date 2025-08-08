-- The array size is the result of a division.
--
-- This was a problem with futhark-py and futhark-pyopencl due to the magic '/'
-- Python 3 division operator.
-- ==
-- input { 5i64 2i64 }
-- output { [0i64, 1i64] }

def main (x: i64) (y: i64) : []i64 =
  iota (x / y)
