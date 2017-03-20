-- ==
-- input { 0u64 1u64 } output { 1u64 0u64 }
-- input { 1u64 1u64 } output { 1u64 1u64 }
-- input { 18446744073709551615u64 1u64 } output { 18446744073709551615u64 1u64 }
-- input { 1u64 18446744073709551615u64 } output { 18446744073709551615u64 1u64 }

import "futlib/math"

fun main(x: u64) (y: u64): (u64,u64) =
  (u64.max x y,
   u64.min x y)
