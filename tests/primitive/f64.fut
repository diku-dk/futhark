-- Test ad-hoc properties and utility functions for f64.
--
-- ==
-- input {  1f64 0f64 } output { true false 0x3ff0000000000000u64 1f64 }
-- input { -1f64 0f64 } output { true true 0xbff0000000000000u64 -1f64 }
-- input { -1f64 1f64 } output { false true 0xbff0000000000000u64 -1f64 }

import "/futlib/math"

let main (x: f64) (y: f64): (bool, bool, u64, f64) =
  (f64.isinf(x / y),
   f64.isnan(f64.sqrt(x)),
   f64.to_bits x,
   f64.from_bits (f64.to_bits x))
