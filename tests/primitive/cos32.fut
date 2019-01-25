-- Does the cos32 function work?
-- ==
-- input { 0f32 } output { 1f32 }
-- input { -1f32 } output { 0.5403023f32 }
-- input { 3.1415927f32 } output { -1f32 }
-- input { -3.1415927f32 } output { -1f32 }


let main(x: f32): f32 = f32.cos(x)
