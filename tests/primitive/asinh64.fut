-- Does the f64.asinh function work?
-- ==
-- input { [0f64, -0.84147096f64, -8.742278e-8f64, 8.742278e-8f64] }
-- output { [0f64, -0.7647251350294384f64, -8.742277999999989e-08f64, 8.742277999999989e-08f64] }

let main = map f64.asinh
