-- ==
-- entry: tanpi64
-- input { [0f64, 0.1f64, 1f64, 2f64 ] }
-- output { [0f64, 0.3249196962329063f64, 0f64, 0f64] }

-- ==
-- entry: tanpi32
-- input { [0f32, 0.1f32, 1f32, 2f32 ] }
-- output { [0f32, 0.3249197f32, 0f32, 0f32] }

-- ==
-- entry: tanpi16
-- input { [0f16, 0.1f16, 1f16, 2f16 ] }
-- output { [0f16, 0.32470703f16, 0f16, 0f16 ] }

entry tanpi64 = map f64.tanpi
entry tanpi32 = map f32.tanpi
entry tanpi16 = map f16.tanpi
