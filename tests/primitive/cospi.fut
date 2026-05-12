-- ==
-- entry: cospi64
-- input { [0f64, 0.5f64, 1f64, 2f64, -1f64 ] }
-- output { [1.0, 0f64, -1.0, 1.0, -1.0] }

-- ==
-- entry: cospi32
-- input { [0f32, 0.5f32, 1f32, 2f32, -1f32 ] }
-- output { [1.0f32, 0f32, -1.0f32, 1.0f32, -1.0f32] }

-- ==
-- entry: cospi16
-- input { [0f16, 0.5f16, 1f16, 2f16, -1f16 ] }
-- output { [1.0f16, 0f16, -1.0f16, 1.0f16, -1.0f16] }

entry cospi64 = map f64.cospi
entry cospi32 = map f32.cospi
entry cospi16 = map f16.cospi
