-- NaN and inf must work.
-- ==
-- input { 2f64 }
-- output { false true true true true }

import "futlib/numeric"

fun main(x: f64) =
  (x < f64.nan,
   x < f64.inf,
   x - f64.inf < x + f64.inf,
   f64.isnan (x + f64.nan),
   f64.isinf (x + f64.inf))
