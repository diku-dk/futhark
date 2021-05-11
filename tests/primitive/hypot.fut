-- ==
-- entry: hypotf32
-- input { [0f32, 3f32] [0f32, 4f32] } output { [0f32, 5f32] }

-- ==
-- entry: hypotf64
-- input { [0f64, 3f64] [0f64, 4f64] } output { [0f64, 5f64] }

entry hypotf32 = map2 f32.hypot
entry hypotf64 = map2 f64.hypot
