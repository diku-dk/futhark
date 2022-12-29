-- NaN and inf must work.

-- ==
-- entry: eqNaN
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: ltNaN
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: lteNaN
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [false, false, false, false] }

-- ==
-- entry: ltInf
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [true, false, false, true] }

-- ==
-- entry: lteInf
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [true, false, true, true] }

-- ==
-- entry: diffInf
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [true, false, false, false] }

-- ==
-- entry: sumNaN
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [true, true, true, true] }

-- ==
-- entry: sumInf
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [true, false, true, false] }

-- ==
-- entry: log2
-- input { [2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [false, true, false, true] }

-- ==
-- entry: log10
-- input { [10f64, f64.nan, f64.inf, -f64.inf] }
-- output { [false, true, false, true] }

-- ==
-- entry: log1p
-- input { [-2f64, -1f64, 2f64, f64.nan, f64.inf, -f64.inf] }
-- output { [true, false, false, true, false, true] }

entry eqNaN = map (\x -> x == f64.nan)
entry ltNaN = map (\x -> x < f64.nan)
entry lteNaN = map (\x -> x <= f64.nan)
entry ltInf = map (\x -> x < f64.inf)
entry lteInf = map (\x -> x <= f64.inf)
entry diffInf = map (\x -> x - f64.inf < x + f64.inf)
entry sumNaN = map (\x -> f64.isnan (x + f64.nan))
entry sumInf = map (\x -> f64.isinf (x + f64.inf))
entry log2 = map (\x -> f64.isnan (f64.log2 (x)))
entry log10 = map (\x -> f64.isnan (f64.log10 (x)))
entry log1p = map (\x -> f64.isnan (f64.log1p (x)))
