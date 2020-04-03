-- Does the f32.sinh function work?
-- ==
-- input  { [0f32, -1f32, 3.1415927f32, -3.1415927f32] }
-- output { [0f32, -1.1752011936438014f32, 11.548739357257748f32, -11.548739357257748f32] }

let main = map f32.sinh
