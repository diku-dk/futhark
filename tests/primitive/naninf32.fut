-- NaN and inf must work.

-- ==
-- entry: eqNaN
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: ltNaN
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: lteNaN
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: ltInf
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [true, false, false, true] }

-- ==
-- entry: lteInf
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [true, false, true, true] }

-- ==
-- entry: diffInf
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [true, false, false, false] }

-- ==
-- entry: sumNaN
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [true, true, true, true] }

-- ==
-- entry: sumInf
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [true, false, true, false] }

-- ==
-- entry: log2
-- input { [2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [false, true, false, true] }

-- ==
-- entry: log10
-- input { [10f32, f32.nan, f32.inf, -f32.inf] }
-- output { [false, true, false, true] }

-- ==
-- entry: log1p
-- input { [-2f32, -1f32, 2f32, f32.nan, f32.inf, -f32.inf] }
-- output { [true, false, false, true, false, true] }

entry eqNaN = map (\x -> x == f32.nan)
entry ltNaN = map (\x -> x < f32.nan)
entry lteNaN = map (\x -> x <= f32.nan)
entry ltInf = map (\x -> x < f32.inf)
entry lteInf = map (\x -> x <= f32.inf)
entry diffInf = map (\x -> x - f32.inf < x + f32.inf)
entry sumNaN = map (\x -> f32.isnan (x + f32.nan))
entry sumInf = map (\x -> f32.isinf (x + f32.inf))
entry log2 = map (\x -> f32.isnan (f32.log2 (x)))
entry log10 = map (\x -> f32.isnan (f32.log10 (x)))
entry log1p = map (\x -> f32.isnan (f32.log1p (x)))
