-- Test ad-hoc properties and utility functions for f64.
--
-- ==
-- input {  1f64 0f64 } output { true false }
-- input { -1f64 0f64 } output { true true }
-- input { -1f64 1f64 } output { false true }

import "/futlib/math"

let main(x: f64, y: f64): (bool, bool) =
  (f64.isinf(x / y), f64.isnan(f64.sqrt(x)))
