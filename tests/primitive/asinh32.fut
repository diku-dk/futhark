-- Does the f32.asinh function work?
-- ==
-- input { [0f32, -0.84147096f32, -8.742278e-8f32, 8.742278e-8f32] }
-- output { [0f32, -0.7647251350294384f32, -8.742277999999989e-08f32, 8.742277999999989e-08f32] }

let main = map f32.asinh
