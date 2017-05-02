-- NaN and inf must work.
-- ==
-- input { 2f32 }
-- output { false true true true true }

import "/futlib/math"

let main(x: f32) =
  (x < f32.nan,
   x < f32.inf,
   x - f32.inf < x + f32.inf,
   f32.isnan (x + f32.nan),
   f32.isinf (x + f32.inf))
