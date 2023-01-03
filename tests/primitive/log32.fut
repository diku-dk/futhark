-- ==
-- entry: logf32
-- input { [0.0f32, 2.718281828459045f32, 2f32, 10f32, f32.inf] }
-- output { [-f32.inf, 1f32, 0.6931471805599453f32, 2.302585092994046f32, f32.inf] }

-- ==
-- entry: log2f32
-- input { [0.0f32, 2.718281828459045f32, 2f32, 10f32, f32.inf] }
-- output { [-f32.inf, 1.4426950408889634f32, 1f32, 3.321928094887362f32, f32.inf] }

-- ==
-- entry: log10f32
-- input { [0.0f32, 2.718281828459045f32, 2f32, 10f32, f32.inf] }
-- output { [-f32.inf, 0.4342944819032518f32, 0.3010299956639812f32, 1f32, f32.inf] }

-- ==
-- entry: log1pf32
-- input { [-1.0f32, -1e-12f32, 0.0f32, 1e-23f32, 1.718281828459045f32, 1f32, f32.inf] }
-- output { [-f32.inf, -1e-12f32, 0.0f32, 1e-23f32, 1.0f32, 0.6931471805599453f32, f32.inf] }

entry logf32 = map f32.log
entry log2f32 = map f32.log2
entry log10f32 = map f32.log10
entry log1pf32 = map f32.log1p
