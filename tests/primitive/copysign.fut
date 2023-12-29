-- ==
-- entry: test_f16
-- input  { [1f16,  1f16,  f16.nan,  f16.inf]
--          [2f16, -2f16, -1f16,    -1f16] }
-- output { [1f16, -1f16,  f16.nan, -f16.inf] }

-- ==
-- entry: test_f32
-- input  { [1f32,  1f32,  f32.nan,  f32.inf]
--          [2f32, -2f32, -1f32,    -1f32] }
-- output { [1f32, -1f32,  f32.nan, -f32.inf] }

-- ==
-- entry: test_f64
-- input  { [1f64,  1f64,  f64.nan,  f64.inf]
--          [2f64, -2f64, -1f64,    -1f64] }
-- output { [1f64, -1f64,  f64.nan, -f64.inf] }

entry test_f16 = map2 f16.copysign
entry test_f32 = map2 f32.copysign
entry test_f64 = map2 f64.copysign
