-- Test unary operators for i16.

-- ==
-- entry: negatei16
-- input { [0i16, 1i16, -1i16, 8i16, -8i16] }
-- output { [0i16, -1i16, 1i16, -8i16, 8i16] }

-- ==
-- entry: absi16
-- input { [0i16, 1i16, -1i16, 8i16, -8i16] }
-- output { [0i16, 1i16, 1i16, 8i16, 8i16] }

-- ==
-- entry: sgni16
-- input { [0i16, 1i16, -1i16, 8i16, -8i16] }
-- output { [0i16, 1i16, -1i16, 1i16, -1i16] }

entry negatei16 = map (\x : i16 -> -x)
entry absi16 = map (i16.abs)
entry sgni16 = map (i16.sgn)
