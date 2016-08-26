-- Test comparison of i64 values.
--
-- ==
-- input {  0i64  0i64 } output { False True True }
-- input {  1i64  2i64 } output { True False True }
-- input { -1i64  1i64 } output { True False True }
-- input {  1i64 -1i64 } output { False False False }
-- input { -2i64 -1i64 } output { True False True }

fun main(x: i64, y: i64): (bool, bool, bool) =
  (x < y, x == y, x <= y)
