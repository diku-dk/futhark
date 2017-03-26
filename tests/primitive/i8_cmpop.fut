-- Test comparison of i8 values.
--
-- ==
-- input {  0i8  0i8 } output { false true true }
-- input {  1i8  2i8 } output { true false true }
-- input { -1i8  1i8 } output { true false true }
-- input {  1i8 -1i8 } output { false false false }
-- input { -2i8 -1i8 } output { true false true }

let main(x: i8, y: i8): (bool, bool, bool) =
  (x < y, x == y, x <= y)
