-- ==
-- entry: erf64
-- input { [0f64, 1f64] }
-- output { [0f64, 0.8427007929497149f64] }

-- ==
-- entry: erf32
-- input { [0f32, 1f32] }
-- output { [0f32, 0.8427007929497149f32] }

-- ==
-- entry: erf16
-- input { [0f16, 1f16] }
-- output { [0f16, 0.8427007929497149f16] }

entry erf64 = map f64.erf
entry erf32 = map f32.erf
entry erf16 = map f16.erf
