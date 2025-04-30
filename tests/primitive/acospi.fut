-- ==
-- entry: acospi64
-- input { [1.0, 0f64, -1.0] }
-- output { [0f64, 0.5f64, 1f64 ] }

-- ==
-- entry: acospi32
-- input { [1.0f32, 0f32, -1.0f32] }
-- output { [0f32, 0.5f32, 1f32 ] }

-- ==
-- entry: acospi16
-- input { [1.0f16, 0f16, -1.0f16] }
-- output { [0f16, 0.5f16, 1f16 ] }

entry acospi64 = map f64.acospi
entry acospi32 = map f32.acospi
entry acospi16 = map f16.acospi
