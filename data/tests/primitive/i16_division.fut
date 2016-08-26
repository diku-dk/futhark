-- Test of division-like operators for i16 values.
--
-- ==
-- input {  7i16  3i16 } output {  2i16  1i16  2i16  1i16 }
-- input { -7i16  3i16 } output { -3i16  2i16 -2i16 -1i16 }
-- input { 7i16  -3i16 } output { -3i16 -2i16 -2i16  1i16 }
-- input { -7i16 -3i16 } output {  2i16 -1i16  2i16 -1i16 }

fun main(x: i16, y: i16): (i16,i16,i16,i16) =
  (x / y, x % y, x // y, x %% y)
