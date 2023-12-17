-- ==
-- entry: test_f16
-- input { [7f16, 7f16, -0f16, f16.inf, 1f16] [-4, 4, 10, -1, 1000] }
-- output { [0.437500f16, 112f16, -0f16, f16.inf, f16.inf] }

-- ==
-- entry: test_f32
-- input { [7f32, 7f32, -0f32, f32.inf, 1f32] [-4, 4, 10, -1, 1000] }
-- output { [0.437500f32, 112f32, -0f32, f32.inf, f32.inf] }

-- ==
-- entry: test_f64
-- input { [7f64, 7f64, -0f64, f64.inf, 1f64] [-4, 4, 10, -1, 10000] }
-- output { [0.437500f64, 112f64, -0f64, f64.inf, f64.inf] }

entry test_f16 = map2 f16.ldexp
entry test_f32 = map2 f32.ldexp
entry test_f64 = map2 f64.ldexp
