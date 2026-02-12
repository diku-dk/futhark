-- NaN and inf must work.

-- ==
-- entry: eqNaN
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: ltNaN
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: lteNaN
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: ltInf
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [true, false, false, true] }

-- ==
-- entry: lteInf
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [true, false, true, true] }

-- ==
-- entry: diffInf
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [true, false, false, false] }

-- ==
-- entry: sumNaN
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [true, true, true, true] }

-- ==
-- entry: sumInf
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [true, false, true, false] }

-- ==
-- entry: log2
-- input { [2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [false, true, false, true] }

-- ==
-- entry: log10
-- input { [10f16, f16.nan, f16.inf, -f16.inf] }
-- output { [false, true, false, true] }

-- ==
-- entry: log1p
-- input { [-2f16, -1f16, 2f16, f16.nan, f16.inf, -f16.inf] }
-- output { [true, false, false, true, false, true] }

entry eqNaN = map (\x -> x == f16.nan)
entry ltNaN = map (\x -> x < f16.nan)
entry lteNaN = map (\x -> x <= f16.nan)
entry ltInf = map (\x -> x < f16.inf)
entry lteInf = map (\x -> x <= f16.inf)
entry diffInf = map (\x -> x - f16.inf < x + f16.inf)
entry sumNaN = map (\x -> f16.isnan (x + f16.nan))
entry sumInf = map (\x -> f16.isinf (x + f16.inf))
entry log2 = map (\x -> f16.isnan (f16.log2 (x)))
entry log10 = map (\x -> f16.isnan (f16.log10 (x)))
entry log1p = map (\x -> f16.isnan (f16.log1p (x)))
