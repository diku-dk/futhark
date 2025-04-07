-- ==
-- entry: asinpi64
-- input { [0.0f64, 1f64, -1f64, -1.0f64] }
-- output { [0f64, 0.5f64, -0.5f64, -0.5f64 ] }

-- ==
-- entry: asinpi32
-- input { [0.0f32, 1f32, -1f32, -1.0f32] }
-- output { [0f32, 0.5f32, -0.5f32, -0.5f32 ] }

-- ==
-- entry: asinpi16
-- input { [0.0f16, 1f16, -1f16, -1.0f16] }
-- output { [0f16, 0.5f16, -0.5f16, -0.5f16 ] }

entry asinpi64 = map f64.asinpi
entry asinpi32 = map f32.asinpi
entry asinpi16 = map f16.asinpi
