-- Test comparison of u64 values.
--
-- ==
-- input {  0u64  0u64 } output { False True True }
-- input {  1u64  2u64 } output { True False True }
-- input { 18446744073709551615u64 1u64 } output { False False False }
-- input {  1u64 18446744073709551615u64 } output { True False True }

fun (bool, bool, bool) main(u64 x, u64 y) =
  (x < y, x == y, x <= y)
