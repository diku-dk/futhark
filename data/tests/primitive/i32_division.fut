-- Test of division-like operators for i32 values.
--
-- ==
-- input {  7i32  3i32 } output {  2i32  1i32  2i32  1i32 }
-- input { -7i32  3i32 } output { -3i32  2i32 -2i32 -1i32 }
-- input { 7i32  -3i32 } output { -3i32 -2i32 -2i32  1i32 }
-- input { -7i32 -3i32 } output {  2i32 -1i32  2i32 -1i32 }

fun (i32,i32,i32,i32) main(i32 x, i32 y) =
  (x / y, x % y, x // y, x %% y)
