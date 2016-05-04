-- Test ad-hoc properties and utility functions for f32.
--
-- ==
-- input {  1f32 0f32 } output { True False }
-- input { -1f32 0f32 } output { True True }
-- input { -1f32 1f32 } output { False True }

fun (bool, bool) main(f32 x, f32 y) =
  (isinf32(x / y), isnan32(sqrt32(x)))
