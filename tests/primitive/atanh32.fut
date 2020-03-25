-- Does the f32.atanh function work?
-- ==
-- input { [0f32, 0.5f32, 1f32, -1f32] }
-- output { [0f32, 0.5493061443340548f32, f32.inf, -f32.inf] }

let main = map f32.atanh
