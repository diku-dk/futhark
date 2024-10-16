def pow_list [n] y (xs :[n]i32) =
  #[stripmine(2)]
  loop accs = (replicate n 1) for _i < y do
          map2 (*) accs xs

-- ==
-- entry: prim
-- input { 3 [1,2,3] } output { [1,8,27] }
entry prim y xs = pow_list y xs

-- ==
-- entry: f_vjp f_jvp
-- input { 3 [1,2,3] }
-- output { [[3,0,0],
--           [0,12,0],
--           [0,0,27]]
--        }
entry f_jvp [n] y (xs :[n]i32) =
  tabulate n (\i -> jvp (pow_list y) xs (replicate n 0 with [i] = 1)) |> transpose
entry f_vjp [n] y (xs :[n]i32) =
  tabulate n (\i -> vjp (pow_list y) xs (replicate n 0 with [i] = 1))
