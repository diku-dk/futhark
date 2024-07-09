def test [n] (xs: [n]i32) =
  let xs' = copy xs
  let foo = xs' with [1] =  i32.sum xs'
  in map (*2) foo

-- ==
-- entry: f_vjp
-- compiled input { [1, 2, 3] }
-- output { [4, 2, 4] }
entry f_vjp [n] (xs: [n]i32) = vjp test xs (replicate n 1)
