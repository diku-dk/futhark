-- ==
-- entry: gamma64
-- input { [1.0, 4.0] }
-- output { [1f64, 6f64] }

-- ==
-- entry: gamma32
-- input { [1f32, 4f32] }
-- output { [1f32, 6f32] }

entry gamma64 = map f64.gamma
entry gamma32 = map f32.gamma
