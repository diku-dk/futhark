-- Does the f32.cosh function work?
-- ==
-- input  { [0f32, -1f32, 3.1415927f32, -3.1415927f32] }
-- output { [1.0f32, 1.5430806348152437f32, 11.591953275521519f32, 11.591953275521519f32] }

let main = map f32.cosh
