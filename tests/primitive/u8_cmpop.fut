-- Test comparison of u8 values.
--
-- ==
-- input {  0u8  0u8 } output { false true true }
-- input {  1u8  2u8 } output { true false true }
-- input { 255u8 1u8 } output { false false false }
-- input {  1u8 255u8 } output { true false true }

let main (x: u8) (y: u8): (bool, bool, bool) =
  (x < y, x == y, x <= y)
