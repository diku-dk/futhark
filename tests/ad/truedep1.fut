-- ==
-- tags { autodiff }

entry test [n] [m] (xss: [n][m]f32) =
  loop #[true_dep] xss' = copy xss
  for i < n do
    loop #[true_dep] xss'' = copy xss'
    for j < m do
      xss'' with [i, j] = xss''[i - 1, j - 1] * xss''[i - 1, j] * xss''[i, j - 1]

-- ==
-- entry: prim
entry prim [n] [m] (xss: [n][m]f32) = test xss

-- ==
-- entry: f_vjp
entry f_vjp [n] [m] (xss: [n][m]f32) = vjp test xss ((replicate n (replicate m 0)) with [0, 0] = 1)
