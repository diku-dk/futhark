-- Test comparison of i32 values.
--
-- ==
-- input {  0i32  0i32 } output { False True True }
-- input {  1i32  2i32 } output { True False True }
-- input { -1i32  1i32 } output { True False True }
-- input {  1i32 -1i32 } output { False False False }
-- input { -2i32 -1i32 } output { True False True }

fun main(x: i32, y: i32): (bool, bool, bool) =
  (x < y, x == y, x <= y)
