-- Does the f64.tanh function work?
-- ==
-- input  { [0f64, 0.78539819f64, -0.78539819f64] }
-- output { [0f64, 0.6557942177943699f64, -0.6557942177943699f64] }

let main = map f64.tanh
