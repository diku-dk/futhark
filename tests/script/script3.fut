-- ==
-- tags { no_webgpu }
-- entry: dotprod
-- script input @ script3.futharkscript

entry dotprod (xs: []f64) (ys: []f64) = f64.sum (map2 (*) xs ys)
