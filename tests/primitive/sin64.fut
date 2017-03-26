-- Does the sin64 function work?
-- ==
-- input { 0.0 } output { 0.0 }
-- input { -1.0 } output { -0.84147096 }
-- input { 3.1415927 } output { -8.742278e-8 }
-- input { -3.1415927 } output { 8.742278e-8 }

import "futlib/math"

let main(x: f64): f64 = f64.sin(x)
