-- Test of division-like operators for i8 values.
--
-- ==
-- input {  7i8  3i8 } output {  2i8  1i8  2i8  1i8 }
-- input { -7i8  3i8 } output { -3i8  2i8 -2i8 -1i8 }
-- input { 7i8  -3i8 } output { -3i8 -2i8 -2i8  1i8 }
-- input { -7i8 -3i8 } output {  2i8 -1i8  2i8 -1i8 }

let main(x: i8, y: i8): (i8,i8,i8,i8) =
  (x / y, x % y, x // y, x %% y)
