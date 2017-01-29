-- Test ad-hoc properties and utility functions for f32.
--
-- ==
-- input {  1f32 0f32 } output { true false }
-- input { -1f32 0f32 } output { true true }
-- input { -1f32 1f32 } output { false true }

import "futlib/numeric"

fun main(x: f32, y: f32): (bool, bool) =
  (f32.isinf(x / y), f32.isnan(f32.sqrt(x)))
