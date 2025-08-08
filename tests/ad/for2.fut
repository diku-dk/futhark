def mult_list xs =
  loop start = 1
  for x in xs do
    x * x

-- ==
-- entry: prim
-- input { [11,5,13] } output { 169 }
entry prim = mult_list

-- ==
-- entry: f_jvp f_vjp
-- input { [11,5,13] } output { [0,0,26] }
entry f_jvp [n] (xs: [n]i32) =
  tabulate n (\i -> jvp mult_list xs (replicate n 0 with [i] = 1))

entry f_vjp [n] (xs: [n]i32) = vjp mult_list xs 1
