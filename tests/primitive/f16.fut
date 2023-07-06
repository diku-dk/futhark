-- Test ad-hoc properties and utility functions for f16.

-- ==
-- entry: testInf
-- input { [1f16, -1f16, -1f16] [0f16, 0f16, 1f16] }
-- output { [true, true, false] }

-- ==
-- entry: testNaN
-- input { [1f16, -1f16, -1f16] [0f16, 0f16, 1f16] }
-- output { [false, true, true] }

-- ==
-- entry: testToBits
-- input { [1f16, -1f16, -1f16] [0f16, 0f16, 1f16] }
-- output { [0x3c00u16, 0xbc00u16, 0xbc00u16] }

-- ==
-- entry: testFromBits
-- input { [1f16, -1f16, -1f16] [0f16, 0f16, 1f16] }
-- output { [1f16, -1f16, -1f16] }

-- ==
-- entry: testNeg
-- input { [1f16, f16.inf, -f16.inf, f16.nan] }
-- output { [-1f16, -f16.inf, f16.inf, f16.nan] }

entry testInf (xs: []f16) (ys: []f16) = map2 (\x y -> f16.isinf(x/y)) xs ys
entry testNaN (xs: []f16) (ys: []f16) = map (\x -> f16.isnan(f16.sqrt(x))) xs
entry testToBits (xs: []f16) (ys: []f16) = map f16.to_bits xs
entry testFromBits (xs: []f16) (ys: []f16) = map (\x -> f16.from_bits(f16.to_bits(x))) xs
entry testNeg = map f16.neg
