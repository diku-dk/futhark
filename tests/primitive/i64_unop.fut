-- Test unary operators for i64.
--
-- ==
-- entry: neg
-- input { [0i64, 1i64, -1i64, 8i64, -8i64]}
-- output { [0i64,-1i64,1i64,-8i64,8i64] }

-- ==
-- entry: abs
-- input { [0i64, 1i64, -1i64, 8i64, -8i64, 5000000000i64, -5000000000i64] }
-- output { [0i64,1i64,1i64,8i64,8i64, 5000000000i64, 5000000000i64] }

-- ==
-- entry: sgn
-- input { [0i64,1i64,-1i64,8i64,-8i64] }
-- output { [0i64,1i64,-1i64,1i64,-1i64] }

entry neg = map i64.neg
entry abs = map i64.abs
entry sgn = map i64.sgn
