-- Test ad-hoc properties and utility functions for f32.
--
-- ==
-- input {  1f32 0f32 } output { True False }
-- input { -1f32 0f32 } output { True True }
-- input { -1f32 1f32 } output { False True }

fun main(x: f32, y: f32): (bool, bool) =
  (isinf32(x / y), isnan32(sqrt32(x)))
