-- ==
-- tags { autodiff }

entry main (xs: *[3]f64) (is: [1]i64) (vs: [1]f64) =
  vjp (\vs -> scatter (copy xs) is vs) vs [1, 0, 0]
