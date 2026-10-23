-- ==
-- tags { autodiff }

def test [n] (xs: [n]f32) =
  let xs' = copy xs
  let xs'' = copy xs
  in xs' with [1] = xs''[1]

-- ==
-- entry: prim
-- input { [5.0f32, 7.0f32, 9.0f32] }
-- output { [5.0f32, 7.0f32, 9.0f32] }
entry prim [n] (xs: [n]f32) = test xs

-- ==
-- entry: f_vjp
-- input { [5.0f32, 7.0f32, 9.0f32] }
-- output { [1.0f32, 1.0f32, 1.0f32] }
entry f_vjp [n] (xs: [n]f32) = vjp test xs (replicate n 1)
