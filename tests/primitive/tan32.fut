-- Does the tan32 function work?
-- ==
-- input { 0f32 } output { 0f32 }
-- input { 0.78539819f32 } output { 1f32 }
-- input { -0.78539819f32 } output { -1f32 }

let main(x: f32): f32 = f32.tan x
