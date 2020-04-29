-- Does the f32.acosh function work?
-- ==
-- input { [1f32, 0.5403023f32, 3.14f32] }
-- output { [0f32,  f32.nan, 1.810991348900196f32 ] }

let main = map f32.acosh
