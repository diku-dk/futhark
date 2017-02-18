-- Does the acos32 function work?
-- ==
-- input { 1f32 } output { 0f32 }
-- input { 0.5403023f32 } output { 1f32 }
-- input { -1f32 } output { 3.1415927f32 }

import "futlib/math"

fun main(x: f32): f32 = f32.acos(x)
