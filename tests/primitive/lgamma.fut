-- ==
-- entry: lgammaf32
-- input { [1.0f32, 4.0f32] }
-- output { [0f32, 1.7917594692280554f32] }

-- ==
-- entry: lgammaf64
-- input { [1.0, 4.0] }
-- output { [0f64, 1.7917594692280554f64] }

entry lgammaf32 = map f32.lgamma
entry lgammaf64 = map f64.lgamma
