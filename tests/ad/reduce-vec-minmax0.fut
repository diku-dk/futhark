-- ==
-- entry: main
-- compiled random input { [50][66]f32 } output { true }
-- compiled random input { [23][45]f32} output { true }

def redmap [n] [m] (arr: [m][n]f32) : [n]f32 =
  reduce (map2 f32.max) (replicate n f32.lowest) arr

def forward [n] [m] (arr: [m][n]f32) : [n][m][n]f32 =
  tabulate_2d m n (\i j -> jvp redmap arr (replicate m (replicate n 0) with [i] = (replicate n 0 with [j] = 1)))
  |> transpose

def reverse [n] [m] (arr: [m][n]f32) : [n][m][n]f32 =
  tabulate n (\i -> vjp redmap arr (replicate n 0 with [i] = 1))

def main [n] [m] (arr: [m][n]f32) : bool =
  let l = n * m * n
  let fs = forward arr |> flatten_3d :> [l]f32
  let rs = reverse arr |> flatten_3d :> [l]f32
  in reduce (&&) true (map2 (\i j -> f32.abs (i - j) < 0.0001f32) fs rs)
