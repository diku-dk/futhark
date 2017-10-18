-- Test comparison of u64 values.
--
-- ==
-- input {  0u64  0u64 } output { false true true }
-- input {  1u64  2u64 } output { true false true }
-- input { 18446744073709551615u64 1u64 } output { false false false }
-- input {  1u64 18446744073709551615u64 } output { true false true }

let main(x: u64, y: u64): (bool, bool, bool) =
  (x < y, x == y, x <= y)
