-- Rounding down floats to whole numbers.
-- ==
-- tags { no_csharp }
-- input { 1.0000001192092896f32 } output { 1f32 }
-- input { -0.9999999403953552f32 } output { -1f32 }
-- input { 0f32 } output { 0f32 }
-- input { -0f32 } output { -0f32 }
-- input { 0.49999999999999994f32 } output { 0f32 }
-- input { 0.5f32 } output { 0f32 }
-- input { 1.18e-38f32 } output { 0f32 }
-- input { -f32.inf } output { -f32.inf }
-- input { f32.inf } output { f32.inf }
-- input { f32.nan } output { f32.nan }
-- input { -0f32 } output { -0f32 }

let main = f32.floor
