-- ==
-- entry: hypotf16
-- input { [0f16, 3f16, 1.8446744e19f16] [0f16, 4f16, 0f16] } output { [0f16, 5f16, 1.8446744e19f16] }

-- ==
-- entry: hypotf32
-- input { [0f32, 3f32, 1.8446744e19f32] [0f32, 4f32, 0f32] } output { [0f32, 5f32, 1.8446744e19f32] }

-- ==
-- entry: hypotf64
-- input { [0f64, 3f64, 4.149515568880993e180f64] [0f64, 4f64, 0f64] } output { [0f64, 5f64, 4.149515568880993e180f64] }

entry hypotf16 = map2 f16.hypot
entry hypotf32 = map2 f32.hypot
entry hypotf64 = map2 f64.hypot
