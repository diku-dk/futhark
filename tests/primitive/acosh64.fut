-- Does the f64.acosh function work?
-- ==
-- input { [1f64, 0.5403023f64, 3.14f64] }
-- output { [0f64,  f64.nan, 1.810991348900196f64 ] }

let main = map f64.acosh
