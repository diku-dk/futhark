-- ==
-- entry: dotprod
-- script input @ script3.futharkscript

entry dotprod (xs: []f32) (ys: []f32) = f32.sum (map2 (*) xs ys)
