-- Test of division-like operators for i16 values.
--
-- ==
-- input {  7i16  3i16 } output {  2i16  1i16  2i16  1i16 }
-- input { -7i16  3i16 } output { -3i16  2i16 -2i16 -1i16 }
-- input { 7i16  -3i16 } output { -3i16 -2i16 -2i16  1i16 }
-- input { -7i16 -3i16 } output {  2i16 -1i16  2i16 -1i16 }

fun (i16,i16,i16,i16) main(i16 x, i16 y) =
  (x / y, x % y, x // y, x %% y)
