-- ==
--  entry: rev
--  compiled input {
--    [0i64,1i64,2i64,1i64,0i64,1i64,2i64,1i64]
--    [15f32,1f32,2f32,15f32,16f32]
--    [1f32,2f32,12f32,3f32,2f32,4f32,7f32,5f32] }
--  output {
--    [0f32,1f32,0f32,1f32,0f32]
--    [0f32,1f32,0f32,1f32,0f32,1f32,0f32,1f32] }

let sat_add_u4 (x: f32) (y: f32): f32 =
  let sat_val = f32.i32 ((1 << 4) - 1)
  in if sat_val - x < y
     then sat_val else x + y

def f [n][m] (is: [n]i64) (dst: [m]f32, as: [n]f32) =
  reduce_by_index (copy dst) sat_add_u4 0 is as

entry rev [n][m] (is: [n]i64) (dst: [m]f32) (as: [n]f32) =
  vjp (f is) (dst,as) (replicate m 1)

-- ==
--  entry: revvec
--  compiled input {
--    [0i64,1i64,2i64,1i64,0i64]
--    [[15f32,1f32,2f32],[0f32,16f32,3f32],[15f32,4f32,5f32]]
--    [[1f32,3f32,10f32],[6f32,8f32,3f32],[0f32,11f32,14f32],[7f32,9f32,3f32],[2f32,4f32,5f32]] }
--  output {
--    [[0f32,1f32,0f32],[1f32,0f32,1f32],[1f32,1f32,0f32]]
--    [[0f32,1f32,0f32],[1f32,0f32,1f32],[1f32,1f32,0f32],[1f32,0f32,1f32],[0f32,1f32,0f32]] }

def fvec [k][n][m] (is: [n]i64) (dst: [k][m]f32, as: [n][m]f32) =
  reduce_by_index (copy dst) (map2 sat_add_u4) (replicate m 0) is as

entry revvec [k][n][m] (is: [n]i64) (dst: [k][m]f32) (as: [n][m]f32) =
  vjp (fvec is) (dst,as) (replicate k (replicate m 1))
