def test [n] (xs: [n]i32) =
  let xs' = copy xs
  let xs'' = map (* 2) xs'
  in xs' with [1] = xs''[1]

-- ==
-- entry: prim
-- input { [5, 7, 9] }
-- output { [5, 14, 9] }
entry prim [n] (xs: [n]i32) = test xs

-- ==
-- entry: f_vjp
-- input { [5, 7, 9] }
-- output { [1, 2, 1] }
entry f_vjp [n] (xs: [n]i32) = vjp test xs (replicate n 1)

-- ==
-- entry: f_jvp
-- input { [5, 7, 9] }
-- output { [1, 2, 1] }
entry f_jvp [n] (xs: [n]i32) = jvp test xs (replicate n 1)
