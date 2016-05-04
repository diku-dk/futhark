-- Test comparison of u8 values.
--
-- ==
-- input {  0u8  0u8 } output { False True True }
-- input {  1u8  2u8 } output { True False True }
-- input { 255u8 1u8 } output { False False False }
-- input {  1u8 255u8 } output { True False True }

fun (bool, bool, bool) main(u8 x, u8 y) =
  (x < y, x == y, x <= y)
