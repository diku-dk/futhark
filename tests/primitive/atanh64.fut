-- Does the f64.atanh function work?
-- ==
-- input { [0f64, 0.5f64, 1f64, -1f64] }
-- output { [0f64, 0.5493061443340548f64, f64.inf, -f64.inf] }

let main = map f64.atanh
