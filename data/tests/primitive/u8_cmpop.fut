-- Test comparison of u8 values.
--
-- ==
-- input {  0u8  0u8 } output { False True True }
-- input {  1u8  2u8 } output { True False True }
-- input { 255u8 1u8 } output { False False False }
-- input {  1u8 255u8 } output { True False True }

fun main(x: u8, y: u8): (bool, bool, bool) =
  (x < y, x == y, x <= y)
