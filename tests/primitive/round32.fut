-- Rounding floats to whole numbers.
-- ==
-- input { -0.499999970197677612304687500000f32 } output { -0f32 }
-- input { -0.5f32 } output { -1f32 }
-- input { -0.5000000000000001f32 } output { -1f32 }
-- input { 0f32 } output { 0f32 }
-- input { 0.499999970197677612304687500000f32 } output { 0f32 }
-- input { 0.5f32 } output { 1f32 }
-- input { 0.5000000000000001f32 } output { 1f32 }
-- input { 1.390671161567e-309f32 } output { 0f32 }
-- input { 2.2517998136852485e+15f32 } output { 2.251799813685249e+15f32 }
-- input { 4.503599627370497e+15f32 } output { 4.503599627370497e+15f32 }
-- input { -f32.inf } output { -f32.inf }
-- input { f32.inf } output { f32.inf }
-- input { f32.nan } output { f32.nan }
-- input { -0f32 } output { -0f32 }

let main = f32.round
