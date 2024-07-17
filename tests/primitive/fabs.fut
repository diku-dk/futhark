-- ==
-- entry: test_f16
-- input { [1f16, -1f16, -f16.inf, f16.nan] }
-- output { [1f16, 1f16, f16.inf, f16.nan] }

-- ==
-- entry: test_f32
-- input { [1f32, -1f32, -f32.inf, f32.nan] }
-- output { [1f32, 1f32, f32.inf, f32.nan] }

-- ==
-- entry: test_f64
-- input { [1f64, -1f64, -f64.inf, f64.nan] }
-- output { [1f64, 1f64, f64.inf, f64.nan] }

entry test_f16 = map f16.abs
entry test_f32 = map f32.abs
entry test_f64 = map f64.abs
