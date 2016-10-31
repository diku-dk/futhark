-- Test comparison of u32 values.
--
-- ==
-- input {  0u32  0u32 } output { false true true }
-- input {  1u32  2u32 } output { true false true }
-- input { 4294967295u32 1u32 } output { false false false }
-- input {  1u32 4294967295u32 } output { true false true }

fun main(x: u32, y: u32): (bool, bool, bool) =
  (x < y, x == y, x <= y)
