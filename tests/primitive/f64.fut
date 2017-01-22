-- Test ad-hoc properties and utility functions for f64.
--
-- ==
-- input {  1f64 0f64 } output { true false }
-- input { -1f64 0f64 } output { true true }
-- input { -1f64 1f64 } output { false true }

include futlib.numeric

fun main(x: f64, y: f64): (bool, bool) =
  (F64.isinf(x / y), F64.isnan(F64.sqrt(x)))
