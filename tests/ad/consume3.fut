def test [n] (xs: [n]f64) =
  let xs' = copy xs
  let xs'' = copy xs
  in xs' with [1] = xs''[1]

-- ==
-- entry: prim
-- input { [5.0, 7.0, 9.0] }
-- output { [5.0, 7.0, 9.0] }
entry prim [n] (xs: [n]f64) = test xs

-- ==
-- entry: f_vjp
-- input { [5.0, 7.0, 9.0] }
-- output { [1.0, 1.0, 1.0] }
entry f_vjp [n] (xs: [n]f64) = vjp test xs (replicate n 1)
