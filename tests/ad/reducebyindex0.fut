-- ==
--  entry: main
--  compiled input {
--    [4i64,5i64,2i64,4i64,5i64,2i64,0i64,0i64,4i64,5i64,1i64,4i64,1i64,3i64,3i64,1i64,8i64,-1i64]
--    [1f32,2f32,0f32,4f32,5f32,0f32,9f32,0f32]
--    [11f32,16f32,7f32,0f32,14f32,6f32,2f32,1f32,13f32,0f32,5f32,0f32,3f32,0f32,9f32,4f32,17f32,18f32]
--    [1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32] }
--  output {
--    [8f32,960f32,504f32,0f32,0f32,0f32,7f32,8f32]
--    [0f32,0f32,0f32,0f32,0f32,0f32,4f32,8f32,0f32,0f32,384f32,0f32,640f32,576f32,0f32,480f32,0f32,0f32]
--    [8f32,960f32,0f32,0f32,0f32,0f32,9f32,0f32] }
def f [n][m] (is: [n]i64) (dst: [m]f32,vs: [n]f32,c: [m]f32) =
  let r = reduce_by_index (copy dst) (\x y -> x*y*2) 0.5 is vs
  in map2 (*) r c

def main [n][m] (is: [n]i64) (dst: [m]f32) (vs: [n]f32) (c: [m]f32) =
  vjp (f is) (dst,vs,c) (replicate m 1)