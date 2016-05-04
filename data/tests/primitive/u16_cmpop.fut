-- Test comparison of u16 values.
--
-- ==
-- input {  0u16  0u16 } output { False True True }
-- input {  1u16  2u16 } output { True False True }
-- input { 65535u16 1u16 } output { False False False }
-- input {  1u16 65535u16 } output { True False True }

fun (bool, bool, bool) main(u16 x, u16 y) =
  (x < y, x == y, x <= y)
