-- Does the f32.tanh function work?
-- ==
-- input  { [0f32, 0.78539819f32, -0.78539819f32] }
-- output { [0f32, 0.6557942177943699f32, -0.6557942177943699f32] }

let main = map f32.tanh
