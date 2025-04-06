-- ==
-- entry: sinpi64
-- input { [0f64, 0.5f64, 1f64, 1.5f64, 2f64, -0.5f64 ] }
-- output { [0.0f64, 1f64, 0f64, -1f64, 0f64, -1.0f64] }

-- ==
-- entry: sinpi32
-- input { [0f32, 0.5f32, 1f32, 1.5f32, 2f32, -0.5f32 ] }
-- output { [0.0f32, 1f32, 0f32, -1f32, 0f32, -1.0f32] }

-- ==
-- entry: sinpi16
-- input { [0f16, 0.5f16, 1f16, 1.5f16, 2f16, -0.5f16 ] }
-- output { [0.0f16, 1f16, 0f16, -1f16, 0f16, -1.0f16] }

entry sinpi64 = map f64.sinpi
entry sinpi32 = map f32.sinpi
entry sinpi16 = map f16.sinpi
