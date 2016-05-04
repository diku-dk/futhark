-- Test ad-hoc properties and utility functions for f64.
--
-- ==
-- input {  1f64 0f64 } output { True False }
-- input { -1f64 0f64 } output { True True }
-- input { -1f64 1f64 } output { False True }

fun (bool, bool) main(f64 x, f64 y) =
  (isinf64(x / y), isnan64(sqrt64(x)))
