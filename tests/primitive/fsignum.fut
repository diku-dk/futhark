-- ==
-- entry: test_f32
-- input { [0f32, 10f32, -10f32, f32.inf, -f32.inf, f32.nan] }
-- output { [0f32, 1f32, -1f32, 1f32, -1f32, f32.nan] }

entry test_f32 = map f32.sgn

-- ==
-- entry: test_f64
-- input { [0f64, 10f64, -10f64, f64.inf, -f64.inf, f64.nan] }
-- output { [0f64, 1f64, -1f64, 1f64, -1f64, f64.nan] }
entry test_f64 = map f64.sgn
