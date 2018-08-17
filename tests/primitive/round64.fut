-- Rounding floats to whole numbers.
-- ==
-- input { -0.49999999999999994f64 } output { -0f64 }
-- input { -0.5f64 } output { -0f64 }
-- input { -0.5000000000000001f64 } output { -1f64 }
-- input { 0f64 } output { 0f64 }
-- input { 0.49999999999999994f64 } output { 0f64 }
-- input { 0.5f64 } output { 0f64 }
-- input { 0.500000000000001f64 } output { 1f64 }
-- input { 1.390671161567e-309f64 } output { 0f64 }
-- input { 2.2517998136852485e+15f64 } output { 2.251799813685249e+15f64 }
-- input { 4.503599627370497e+15f64 } output { 4.503599627370497e+15f64 }
-- input { -f64.inf } output { -f64.inf }
-- input { f64.inf } output { f64.inf }
-- input { f64.nan } output { f64.nan }
-- input { -0f64 } output { -0f64 }

let main = f64.round
