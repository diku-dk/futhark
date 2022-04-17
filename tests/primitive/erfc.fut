-- ==
-- entry: erfc64
-- input { [0f64, 1f64] }
-- output { [1f64, 0.15729920705028513f64] }

-- ==
-- entry: erfc32
-- input { [0f32, 1f32] }
-- output { [1f32, 0.15729920705028513f32] }

-- ==
-- entry: erfc16
-- input { [0f16, 1f16] }
-- output { [1f16, 0.15729920705028513f16] }

entry erfc64 = map f64.erfc
entry erfc32 = map f32.erfc
entry erfc16 = map f16.erfc
