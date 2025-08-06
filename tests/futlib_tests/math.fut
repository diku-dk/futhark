-- ==
-- entry: test_f64_ceil
-- input { 3.4 } output { 4.0 }
-- input { 3.8 } output { 4.0 }
-- input { 4.0 } output { 4.0 }
-- input { 0.0 } output { 0.0 }
-- input { -3.9 } output { -3.0 }
-- input { -3.1 } output { -3.0 }
-- input { -4.0 } output { -4.0 }

entry test_f64_ceil (x: f64) = f64.ceil x

-- ==
-- entry: test_f64_floor
-- input { 3.4 } output { 3.0 }
-- input { 3.8 } output { 3.0 }
-- input { 4.0 } output { 4.0 }
-- input { 0.0 } output { 0.0 }
-- input { -3.9 } output { -4.0 }
-- input { -3.1 } output { -4.0 }
-- input { -4.0 } output { -4.0 }

entry test_f64_floor (x: f64) = f64.floor x

-- ==
-- entry: test_f64_trunc
-- input { 3.4 } output { 3.0 }
-- input { 3.8 } output { 3.0 }
-- input { 4.0 } output { 4.0 }
-- input { 0.0 } output { 0.0 }
-- input { -3.9 } output { -3.0 }
-- input { -3.1 } output { -3.0 }
-- input { -4.0 } output { -4.0 }

entry test_f64_trunc (x: f64) = f64.trunc x

-- ==
-- entry: test_f64_round
-- input { 0.0 } output { 0.0 }
-- input { 99.0 } output { 99.0 }
-- input { -5.0 } output { -5.0 }
-- input { 1.1 } output { 1.0 }
-- input { -1.1 } output { -1.0 }
-- input { 1.9 } output { 2.0 }
-- input { -1.9 } output { -2.0 }
-- input { 2.5 } output { 2.0 }
-- input { -2.5 } output { -2.0 }
-- input { 1000001.4999 } output { 1000001.0 }
-- input { -1000001.4999 } output { -1000001.0 }

entry test_f64_round (x: f64) = f64.round x

-- ==
-- entry: test_f32_ceil
-- input { 3.4f32 } output { 4.0f32 }
-- input { 3.8f32 } output { 4.0f32 }
-- input { 4.0f32 } output { 4.0f32 }
-- input { 0.0f32 } output { 0.0f32 }
-- input { -3.9f32 } output { -3.0f32 }
-- input { -3.1f32 } output { -3.0f32 }
-- input { -4.0f32 } output { -4.0f32 }

entry test_f32_ceil (x: f32) = f32.ceil x

-- ==
-- entry: test_f32_floor
-- input { 3.4f32 } output { 3.0f32 }
-- input { 3.8f32 } output { 3.0f32 }
-- input { 4.0f32 } output { 4.0f32 }
-- input { 0.0f32 } output { 0.0f32 }
-- input { -3.9f32 } output { -4.0f32 }
-- input { -3.1f32 } output { -4.0f32 }
-- input { -4.0f32 } output { -4.0f32 }

entry test_f32_floor (x: f32) = f32.floor x

-- ==
-- entry: test_f32_trunc
-- input { 3.4f32 } output { 3.0f32 }
-- input { 3.8f32 } output { 3.0f32 }
-- input { 4.0f32 } output { 4.0f32 }
-- input { 0.0f32 } output { 0.0f32 }
-- input { -3.9f32 } output { -3.0f32 }
-- input { -3.1f32 } output { -3.0f32 }
-- input { -4.0f32 } output { -4.0f32 }

entry test_f32_trunc (x: f32) = f32.trunc x

-- ==
-- entry: test_f32_round
-- input { 0.0f32 } output { 0.0f32 }
-- input { 99.0f32 } output { 99.0f32 }
-- input { -5.0f32 } output { -5.0f32 }
-- input { 1.1f32 } output { 1.0f32 }
-- input { -1.1f32 } output { -1.0f32 }
-- input { 1.9f32 } output { 2.0f32 }
-- input { -1.9f32 } output { -2.0f32 }
-- input { 2.5f32 } output { 2.0f32 }
-- input { -2.5f32 } output { -2.0f32 }
-- input { 1001.4999f32 } output { 1001.0f32 }
-- input { -1001.4999f32 } output { -1001.0f32 }

entry test_f32_round (x: f32) = f32.round x
