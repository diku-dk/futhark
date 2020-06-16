-- Does the f64.cosh function work?
-- ==
-- input  { [0f64, -1f64, 3.1415927f64, -3.1415927f64] }
-- output { [1.0f64, 1.5430806348152437f64, 11.591953275521519f64, 11.591953275521519f64] }

let main = map f64.cosh
