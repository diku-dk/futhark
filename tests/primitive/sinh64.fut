-- Does the f64.sinh function work?
-- ==
-- input  { [0f64, -1f64, 3.1415927f64, -3.1415927f64] }
-- output { [0f64, -1.1752011936438014f64, 11.548739357257748f64, -11.548739357257748f64] }

let main = map f64.sinh
