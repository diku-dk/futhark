-- ==
-- entry: atan2pi64
-- input { [0f64, 1f64, 1f64, 0f64, -1f64, 1f64, -1f64] [0f64, 0f64, 1f64, 1f64, 1f64, -1f64, -1f64] }
-- output { [0.0f64, 0.5f64, 0.25f64, 0.0f64, -0.25f64, 0.75f64, -0.75f64] }

-- ==
-- entry: atan2pi32
-- input { [0f32, 1f32, 1f32, 0f32, -1f32, 1f32, -1f32] [0f32, 0f32, 1f32, 1f32, 1f32, -1f32, -1f32] }
-- output { [0.0f32, 0.5f32, 0.25f32, 0.0f32, -0.25f32, 0.75f32, -0.75f32] }

-- ==
-- entry: atan2pi16
-- input { [1f16, 1f16, 0f16, -1f16, 1f16, -1f16] [0f16, 1f16, 1f16, 1f16, -1f16, -1f16] }
-- output { [0.5f16, 0.25f16, 0.0f16, -0.25f16, 0.75f16, -0.75f16] }

entry atan2pi64 = map2 f64.atan2pi
entry atan2pi32 = map2 f32.atan2pi
entry atan2pi16 = map2 f16.atan2pi
