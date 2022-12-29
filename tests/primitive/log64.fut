-- ==
-- entry: logf64
-- input { [0.0f64, 2.718281828459045f64, 2f64, 10f64, f64.inf] }
-- output { [-f64.inf, 1f64, 0.6931471805599453f64, 2.302585092994046f64, f64.inf] }

-- ==
-- entry: log2f64
-- input { [0.0f64, 2.718281828459045f64, 2f64, 10f64, f64.inf] }
-- output { [-f64.inf, 1.4426950408889634f64, 1f64, 3.321928094887362f64, f64.inf] }

-- ==
-- entry: log10f64
-- input { [0.0f64, 2.718281828459045f64, 2f64, 10f64, f64.inf] }
-- output { [-f64.inf, 0.4342944819032518f64, 0.3010299956639812f64, 1f64, f64.inf] }

-- ==
-- entry: log1pf64
-- input { [-1.0f64, -1e-123f64, 0.0f64, 1e-234f64, 1.718281828459045f64, 1f64, f64.inf] }
-- output { [-f64.inf, -1e-123f64, 0.0f64, 1e-234f64, 1.0f64, 0.6931471805599453f64, f64.inf] }

entry logf64 = map f64.log
entry log2f64 = map f64.log2
entry log10f64 = map f64.log10
entry log1pf64 = map f64.log1p
