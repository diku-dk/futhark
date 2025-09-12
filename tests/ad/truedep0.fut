-- ==
-- tags { autodiff }

def test [n] (xs: [n]i32) =
  loop #[true_dep] xs' = copy xs
  for i < (n - 1) do
    xs' with [i + 1] = xs'[i] * xs'[i]

-- ==
-- entry: prim
-- input { [2,2,3,4,5] } output { [2,4,16,256,65536] }
entry prim [n] (xs: [n]i32) = test xs

-- ==
-- entry: f_jvp f_vjp
-- input { [1,2,3,4,5] }
-- output { [[1,0,0,0,0],
--           [2,0,0,0,0],
--           [4,0,0,0,0],
--           [8,0,0,0,0],
--           [16,0,0,0,0]]
--        }
entry f_jvp [n] (xs: [n]i32) =
  tabulate n (\i -> jvp test xs (replicate n 0 with [i] = 1)) |> transpose

entry f_vjp [n] (xs: [n]i32) =
  tabulate n (\i -> vjp test xs (replicate n 0 with [i] = 1))
