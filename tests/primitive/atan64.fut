-- Does the atan32 function work?
-- ==
-- input { 0f64 } output { 0f64 }
-- input { 1f64 } output { 0.78539819f64 }
-- input { -1f64 } output { -0.78539819f64 }

import "futlib/numeric"

fun main(x: f64): f64 = f64.atan x
