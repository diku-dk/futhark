-- Does the atan2_32 function work?
-- ==
-- input { 0f32 0f32 } output { 0f32 }
-- input { 1f32 0f32 } output { 1.570796f32 }
-- input { 1f32 1f32 } output { 0.785398f32  }
-- input { 0f32 1f32 } output { 0.000000f32 }
-- input { -1f32 1f32 } output { -0.785398f32 }
-- input { 1f32 -1f32 } output { 2.356194f32 }
-- input { -1f32 -1f32 } output { -2.356194f32 }

import "/futlib/math"

let main(x: f32, y: f32): f32 = f32.atan2 x y
