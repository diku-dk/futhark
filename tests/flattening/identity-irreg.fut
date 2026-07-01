-- ==
-- entry: main
-- input { [[1.0f32, 2.0f32], [3.0f32, 4.0f32]] }
-- auto output

def main [n] [m] (arr: [m][n]f32) : [m][n]f32 =
  map (\i -> arr[i]) (iota m)
