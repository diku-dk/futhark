-- Test comparison of i64 values.
--
-- ==
-- input {  0i64  0i64 } output { False True True }
-- input {  1i64  2i64 } output { True False True }
-- input { -1i64  1i64 } output { True False True }
-- input {  1i64 -1i64 } output { False False False }
-- input { -2i64 -1i64 } output { True False True }

fun (bool, bool, bool) main(i64 x, i64 y) =
  (x < y, x == y, x <= y)
