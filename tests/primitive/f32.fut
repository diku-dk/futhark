-- Test ad-hoc properties and utility functions for f32.

-- ==
-- entry: testInf
-- input { [1f32, -1f32, -1f32] [0f32, 0f32, 1f32] }
-- output { [true, true, false] }

-- ==
-- entry: testNaN
-- input { [1f32, -1f32, -1f32] [0f32, 0f32, 1f32] }
-- output { [false, true, true] }

-- ==
-- entry: testToBits
-- input { [1f32, -1f32, -1f32] [0f32, 0f32, 1f32] }
-- output { [0x3f800000u32, 0xbf800000u32, 0xbf800000u32] }

-- ==
-- entry: testFromBits
-- input { [1f32, -1f32, -1f32] [0f32, 0f32, 1f32] }
-- output { [1f32, -1f32, -1f32] }

entry testInf (xs: []f32) (ys: []f32) = map2 (\x y -> f32.isinf(x/y)) xs ys
entry testNaN (xs: []f32) (ys: []f32) = map (\x -> f32.isnan(f32.sqrt(x))) xs
entry testToBits (xs: []f32) (ys: []f32) = map f32.to_bits xs
entry testFromBits (xs: []f32) (ys: []f32) = map (\x -> f32.from_bits(f32.to_bits(x))) xs
