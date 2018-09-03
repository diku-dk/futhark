-- The array size is the result of a division.
--
-- This was a problem with futhark-py and futhark-pyopencl due to the magic '/'
-- Python 3 division operator.
-- ==
-- input { 5 2 }
-- output { [0, 1] }

let main (x: i32) (y: i32): []i32 =
  iota (x / y)
