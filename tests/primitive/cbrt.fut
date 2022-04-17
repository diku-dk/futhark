-- ==
-- entry: cbrt64
-- input { [0f64, 8f64] }
-- output { [0f64, 2f64] }

-- ==
-- entry: cbrt32
-- input { [0f32, 8f32] }
-- output { [0f32, 2f32] }

-- ==
-- entry: cbrt16
-- input { [0f16, 8f16] }
-- output { [0f16, 2f16] }

entry cbrt64 = map f64.cbrt
entry cbrt32 = map f32.cbrt
entry cbrt16 = map f16.cbrt
