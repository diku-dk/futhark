-- ==
-- input { [1f32,2f32,3f32] }
-- output { [[1.0f32, 2.0f32, 3.0f32], [3.0f32, 1.0f32, 2.0f32], [2.0f32, 3.0f32, 1.0f32]] }

def main [n] (Irow1: [n]f32) =
  let In: [n][n]f32 = map (\i -> rotate (-i) Irow1) (iota n)
  in In
