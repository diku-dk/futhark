let test [n] (xs: [n]i32) =
  let xs' = copy xs
  let xs'' = map (*2) xs'
  in xs' with [1] =  xs''[1]

-- ==
-- tags { disable }
-- entry: prim
-- compiled input { [5, 7, 9] }
-- output { [10, 7, 9] }
entry prim [n] (xs: [n]i32) = test xs

-- ==
-- entry: f_vjp
-- compiled input { [5, 7, 9] }
-- output { [2, 0, 0] }
entry f_vjp [n] (xs: [n]i32) = vjp test xs (replicate n 1)
