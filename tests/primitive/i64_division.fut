-- Test of division-like operators for i64 values.
--
-- ==
-- input {  7i64  3i64 } output {  2i64  1i64  2i64  1i64 }
-- input { -7i64  3i64 } output { -3i64  2i64 -2i64 -1i64 }
-- input { 7i64  -3i64 } output { -3i64 -2i64 -2i64  1i64 }
-- input { -7i64 -3i64 } output {  2i64 -1i64  2i64 -1i64 }

let main (x: i64) (y: i64): (i64,i64,i64,i64) =
  (x / y, x % y, x // y, x %% y)
