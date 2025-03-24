-- ==
-- entry: rsqrt64
-- input { [0f64, 8f64] }
-- output { [f64.inf, 0.35355338f64] }

-- ==
-- entry: rsqrt32
-- input { [0f32, 8f32] }
-- output { [f32.inf, 0.35355338f32] }

-- ==
-- entry: rsqrt16
-- input { [0f16, 8f16] }
-- output { [f16.inf, 0.35355338f16] }

entry rsqrt64 = map f64.rsqrt
entry rsqrt32 = map f32.rsqrt
entry rsqrt16 = map f16.rsqrt
