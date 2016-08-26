-- Test ad-hoc properties and utility functions for f64.
--
-- ==
-- input {  1f64 0f64 } output { True False }
-- input { -1f64 0f64 } output { True True }
-- input { -1f64 1f64 } output { False True }

fun main(x: f64, y: f64): (bool, bool) =
  (isinf64(x / y), isnan64(sqrt64(x)))
