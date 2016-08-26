-- Test comparison of i8 values.
--
-- ==
-- input {  0i8  0i8 } output { False True True }
-- input {  1i8  2i8 } output { True False True }
-- input { -1i8  1i8 } output { True False True }
-- input {  1i8 -1i8 } output { False False False }
-- input { -2i8 -1i8 } output { True False True }

fun main(x: i8, y: i8): (bool, bool, bool) =
  (x < y, x == y, x <= y)
