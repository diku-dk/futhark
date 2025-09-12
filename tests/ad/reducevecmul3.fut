-- ==
-- tags { autodiff }
-- entry: rev
-- input { [[1f32, 2f32, 3f32, 4f32], [1f32, 1f32, 1f32, 1f32]] 3.0f32 } output { [[0f32, 3f32, 0f32, 0f32],[0f32, 6f32, 0f32, 0f32]] 2f32 }

def red_mult [m] [n] (xs: [m][n]f32, c: f32) =
  reduce (map2 (*)) (replicate n 1) xs |> map (* c)

entry rev [m] [n] (xs: [m][n]f32) (c: f32) =
  vjp red_mult (xs, c) (replicate n 0 with [1] = 1)

def main = rev
