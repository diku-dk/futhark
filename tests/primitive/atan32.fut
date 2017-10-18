-- Does the atan32 function work?
-- ==
-- input { 0f32 } output { 0f32 }
-- input { 1f32 } output { 0.78539819f32 }
-- input { -1f32 } output { -0.78539819f32 }

import "/futlib/math"

let main(x: f32): f32 = f32.atan x
