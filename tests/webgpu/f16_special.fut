-- Test f16 special values and edge cases in WebGPU.
-- Tests NaN, infinity, and denormalized number handling.

-- ==
-- entry: isnan
-- input { [0f16, 1f16, f16.nan, f16.inf, -f16.inf] }
-- output { [false, false, true, false, false] }

-- ==
-- entry: isinf
-- input { [0f16, 1f16, f16.nan, f16.inf, -f16.inf] }
-- output { [false, false, false, true, true] }

-- ==
-- entry: nan_propagation
-- input { [1f16, 2f16, f16.nan, f16.inf]
--         [f16.nan, f16.nan, 1f16, f16.nan] }
-- output { [true, true, true, true] }

-- ==
-- entry: inf_arithmetic
-- input { [f16.inf, f16.inf, -f16.inf, f16.inf]
--         [1f16, f16.inf, 1f16, -f16.inf] }
-- output { [f16.inf, f16.inf, -f16.inf, f16.nan] }

-- ==
-- entry: zero_division
-- input { [1f16, -1f16, 0f16]
--         [0f16, 0f16, 0f16] }
-- output { [f16.inf, -f16.inf, f16.nan] }

-- ==
-- entry: subnormal
-- input { [6.104e-5f16, 6.104e-5f16, 6.0e-8f16] }
-- output { [true, true, true] }

-- ==
-- entry: max_min
-- input { [1f16, f16.inf, f16.nan, -65504f16]
--         [2f16, 1f16, 1f16, 65504f16] }
-- output { [2f16, f16.inf, 1f16, 65504f16] }

entry isnan = map f16.isnan
entry isinf = map f16.isinf
entry nan_propagation (a: []f16) (b: []f16) = map2 (\x y -> f16.isnan (x + y)) a b
entry inf_arithmetic (a: []f16) (b: []f16) = map2 (f16.+) a b
entry zero_division (a: []f16) (b: []f16) = map2 (f16./) a b
entry subnormal (xs: []f16) = map (\x -> x > 0f16 && x < 6.104e-5f16 || x == 6.104e-5f16) xs
entry max_min (a: []f16) (b: []f16) = map2 f16.max a b
