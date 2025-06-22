-- ==
--  input {
--    [0i64,1i64,2i64,3i64,2i64,1i64,0i64,1i64,2i64]
--    [0f64,1f64,2f64,3f64]
--    [2f64,3f64,4f64,5f64,6f64,0f64,8f64,9f64,1f64]
--    [1f64,2f64,3f64,4f64,5f64,6f64,7f64,8f64,9f64] }
--  output {
--    [112f64,0f64,3240f64,20f64]
--    [0f64,0f64,1620f64,12f64,1080f64,2592f64,0f64,0f64,6480f64]
--    [0f64,0f64,2160f64,15f64,1296f64,0f64,0f64,0f64,720f64] }

def f [n][m] (is: [n]i64) (dst: [m]f64,vs: [n]f64,c: [n]f64) =
  let tmp = map2 (*) c vs
  in reduce_by_index (copy dst) (*) 1 is tmp

def main [n][m] (is: [n]i64) (dst: [m]f64) (vs: [n]f64) (c: [n]f64) =
  vjp (f is) (dst,vs,c) (replicate m 1)