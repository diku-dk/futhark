-- Test unary operators for u8.

-- ==
-- entry: negateu8
-- input { [0u8, 1u8, 255u8, 8u8, 248u8] }
-- output { [0u8, 255u8, 1u8, 248u8, 8u8] }

-- ==
-- entry: absu8
-- input { [0u8, 1u8, 255u8, 8u8, 248u8] }
-- output { [0u8, 1u8, 255u8, 8u8, 248u8] }

-- ==
-- entry: sgnu8
-- input { [0u8, 1u8, 255u8, 8u8, 248u8] }
-- output { [0u8, 1u8, 1u8, 1u8, 1u8] }

entry negateu8 = map (\x : u8 -> -x)
entry absu8 = map (u8.abs)
entry sgnu8 = map (u8.sgn)
