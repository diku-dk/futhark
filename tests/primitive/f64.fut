-- Test ad-hoc properties and utility functions for f64.

-- ==
-- entry: testInf
-- input { [1f64, -1f64, -1f64] [0f64, 0f64, 1f64] }
-- output { [true, true, false] }

-- ==
-- entry: testNaN
-- input { [1f64, -1f64, -1f64] [0f64, 0f64, 1f64] }
-- output { [false, true, true] }

-- ==
-- entry: testToBits
-- input { [1f64, -1f64, -1f64] [0f64, 0f64, 1f64] }
-- output { [0x3ff0000000000000u64, 0xbff0000000000000u64, 0xbff0000000000000u64] }

-- ==
-- entry: testFromBits
-- input { [1f64, -1f64, -1f64] [0f64, 0f64, 1f64] }
-- output { [1f64, -1f64, -1f64] }

-- ==
-- entry: testNeg
-- input { [1f64, f64.inf, -f64.inf, f64.nan] }
-- output { [-1f64, -f64.inf, f64.inf, f64.nan] }

entry testInf (xs: []f64) (ys: []f64) = map2 (\x y -> f64.isinf(x/y)) xs ys
entry testNaN (xs: []f64) (ys: []f64) = map (\x -> f64.isnan(f64.sqrt(x))) xs
entry testToBits (xs: []f64) (ys: []f64) = map f64.to_bits xs
entry testFromBits (xs: []f64) (ys: []f64) = map (\x -> f64.from_bits(f64.to_bits(x))) xs
entry testNeg = map f64.neg
