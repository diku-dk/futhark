-- Does the atan32 function work?
-- ==
-- input { 0f64 } output { 0f64 }
-- input { 0.78539819f64 } output { 1f64 }
-- input { -0.78539819f64 } output { -1f64 }

let main(x: f64): f64 = f64.tan x
