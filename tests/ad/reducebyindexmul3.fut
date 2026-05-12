-- ==
-- tags { autodiff }
--  entry: main
--  input {
--    [0i64,0i64,0i64,1i64,1i64,2i64,2i64,2i64,2i64]
--    [[1f32,2f32],[0f32,4f32],[5f32,0f32],[9f32,0f32]]
--    [[1f32,3f32],[2f32,4f32],[18f32,5f32],[6f32,0f32],[7f32,9f32],[0f32,14f32],[11f32,0f32],[0f32,16f32],[13f32,17f32]]
--    [[1f32,2f32],[3f32,4f32],[5f32,6f32],[7f32,8f32]] }
--  output {
--    [[36f32,120f32],[126f32,0f32],[0f32,0f32],[7f32,8f32]]
--    [[36f32,80f32],[18f32,60f32],[2f32,48f32],[0f32,144f32],[0f32,0f32],[0f32,0f32],[0f32,0f32],[0f32,0f32],[0f32,0f32]]
--    [[36f32,120f32],[0f32,0f32],[0f32,0f32],[9f32,0f32]] }

def f [n] [m] [k] (is: [n]i64) (dst: [k][m]f32, vs: [n][m]f32, c: [k][m]f32) =
  let tmp = reduce_by_index (copy dst) (map2 (*)) (replicate m 1) is vs
  in map2 (map2 (*)) tmp c

def main [n] [m] [k] (is: [n]i64) (dst: [k][m]f32) (vs: [n][m]f32) (c: [k][m]f32) =
  vjp (f is) (dst, vs, c) (replicate k (replicate m 1))
