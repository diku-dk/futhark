-- Test ad-hoc properties and utility functions for f32.
--
-- ==
-- input {  1f32 0f32 } output { true false }
-- input { -1f32 0f32 } output { true true }
-- input { -1f32 1f32 } output { false true }

fun main(x: f32, y: f32): (bool, bool) =
  (isinf32(x / y), isnan32(sqrt32(x)))
