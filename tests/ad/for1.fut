-- ==
-- tags { autodiff }

def pow_list [n] y (xs: [n]i32) =
  loop accs = (replicate n 1)
  for _i < y do
    map2 (*) accs xs

-- ==
-- entry: prim
-- input { 3 [1,2,3] } output { [1,8,27] }
entry prim y xs = pow_list y xs

-- ==
-- entry: f_vjp f_jvp f_vjp_vec f_jvp_vec
-- input { 3 [1,2,3] }
-- output { [[3,0,0],
--           [0,12,0],
--           [0,0,27]]
--        }
entry f_jvp [n] y (xs: [n]i32) =
  tabulate n (\i -> jvp (pow_list y) xs (replicate n 0 with [i] = 1)) |> transpose

entry f_vjp [n] y (xs: [n]i32) =
  tabulate n (\i -> vjp (pow_list y) xs (replicate n 0 with [i] = 1))

entry f_jvp_vec [n] y (xs: [n]i32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jvp_vec (pow_list y) xs seeds
  |> transpose

entry f_vjp_vec [n] y (xs: [n]i32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in vjp_vec (pow_list y) xs seeds
