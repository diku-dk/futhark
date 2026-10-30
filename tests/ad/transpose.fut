-- ==
-- tags { autodiff no_webgpu }
-- entry: fwd_vec fwd_map
-- input { [[1.0f32,2.0f32],[3.0f32,4.0f32]] }
-- output { [[[1.0f32, 0.0f32],[0.0f32, 0.0f32]],[[0.0f32, 0.0f32],[1.0f32, 0.0f32]],[[0.0f32, 1.0f32],[0.0f32, 0.0f32]],[[0.0f32, 0.0f32],[0.0f32, 1.0f32]]] }

def f (xs: [][]f32) = transpose xs

entry fwd_vec [n] [m] (xs: [n][m]f32) =
  let seeds =
    tabulate (n * m) (\i -> tabulate (n * m) (\j -> f32.bool (i == j)) |> unflatten)
  in (jmp2 f xs seeds).1

entry fwd_map [n] [m] (xs: [n][m]f32) =
  tabulate (n * m)
           (\i -> jvp f xs (tabulate (n * m) (\j -> f32.bool (i == j)) |> unflatten))
