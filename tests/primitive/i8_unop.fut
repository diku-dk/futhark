-- Test unary operators for i8.

-- ==
-- entry: negatei8
-- input { [0i8, 1i8, -1i8, 8i8, -8i8] }
-- output { [0i8, -1i8, 1i8, -8i8, 8i8] }

-- ==
-- entry: absi8
-- input { [0i8, 1i8, -1i8, 8i8, -8i8] }
-- output { [0i8, 1i8, 1i8, 8i8, 8i8] }

-- ==
-- entry: sgni8
-- input { [0i8, 1i8, -1i8, 8i8, -8i8] }
-- output { [0i8, 1i8, -1i8, 1i8, -1i8] }

entry negatei8 = map (\x : i8 -> -x)
entry absi8 = map (i8.abs)
entry sgni8 = map (i8.sgn)
