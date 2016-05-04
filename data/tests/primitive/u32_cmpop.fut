-- Test comparison of u32 values.
--
-- ==
-- input {  0u32  0u32 } output { False True True }
-- input {  1u32  2u32 } output { True False True }
-- input { 4294967295u32 1u32 } output { False False False }
-- input {  1u32 4294967295u32 } output { True False True }

fun (bool, bool, bool) main(u32 x, u32 y) =
  (x < y, x == y, x <= y)
