-- Does the cos64 function work?
-- ==
-- input { 0.0 } output { 1.0 }
-- input { -1.0 } output { 0.5403023 }
-- input { 3.1415927 } output { -1.0 }
-- input { -3.1415927 } output { -1.0 }

import "futlib/numeric"

fun main(x: f64): f64 = F64.cos(x)
