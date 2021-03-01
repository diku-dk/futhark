-- ==
-- entry: test_f32
-- input { 0f32 } output { 0f32 }
-- input { 10f32 } output { 1f32 }
-- input { -10f32 } output { -1f32 }
-- input { f32.inf } output { 1f32 }
-- input { -f32.inf } output { -1f32 }
-- input { f32.nan } output { f32.nan }

entry test_f32 = f32.sgn

-- ==
-- entry: test_f64
-- input { 0f64 } output { 0f64 }
-- input { 10f64 } output { 1f64 }
-- input { -10f64 } output { -1f64 }
-- input { f64.inf } output { 1f64 }
-- input { -f64.inf } output { -1f64 }
-- input { f64.nan } output { f64.nan }

entry test_f64 = f64.sgn
