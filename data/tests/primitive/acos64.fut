-- Does the acos64 function work?
-- ==
-- input { 1f64 } output { 0f64 }
-- input { 0.5403023f64 } output { 1f64 }
-- input { -1f64 } output { 3.1415927f64 }

fun main(x: f64): f64 = acos64(x)
