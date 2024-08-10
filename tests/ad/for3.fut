def square [n] (xs: [n]i32) =
  let xs' = copy xs
  in loop xs'' = xs' for i < n do
       let a = xs''[i]
       in xs'' with [i] = a * a

-- ==
-- entry: prim
-- input { [1,2,3,4,5] } output { [1,4,9,16,25] }
entry prim [n] (xs: [n]i32) = square xs

-- ==
-- entry: f_jvp f_vjp
-- input { [1,2,3,4,5] }
-- output { [[2,0,0,0,0],
--           [0,4,0,0,0],
--           [0,0,6,0,0],
--           [0,0,0,8,0],
--           [0,0,0,0,10]]
--        }
entry f_jvp [n] (xs :[n]i32) =
  tabulate n (\i -> jvp square xs (replicate n 0 with [i] = 1)) |> transpose
entry f_vjp [n] (xs :[n]i32) =
  tabulate n (\i -> vjp square xs (replicate n 0 with [i] = 1))
