-- Test unary operators for i32.

-- ==
-- entry: negatei32
-- input { [0i32, 1i32, -1i32, 8i32, -8i32] }
-- output { [0i32, -1i32, 1i32, -8i32, 8i32] }

-- ==
-- entry: absi32
-- input { [0i32, 1i32, -1i32, 8i32, -8i32] }
-- output { [0i32, 1i32, 1i32, 8i32, 8i32] }

-- ==
-- entry: sgni32
-- input { [0i32, 1i32, -1i32, 8i32, -8i32] }
-- output { [0i32, 1i32, -1i32, 1i32, -1i32] }

entry negatei32 = map (\x : i32 -> -x)
entry absi32 = map (i32.abs)
entry sgni32 = map (i32.sgn)
