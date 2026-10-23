-- ==
-- tags { autodiff }
--  input {
--    [0i64,1i64,2i64,3i64,2i64,1i64,0i64,1i64,2i64]
--    [0f32,1f32,2f32,3f32]
--    [2f32,3f32,4f32,5f32,6f32,0f32,8f32,9f32,1f32]
--    [1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32,9f32] }
--  output {
--    [112f32,0f32,3240f32,20f32]
--    [0f32,0f32,1620f32,12f32,1080f32,2592f32,0f32,0f32,6480f32]
--    [0f32,0f32,2160f32,15f32,1296f32,0f32,0f32,0f32,720f32] }

def f [n] [m] (is: [n]i64) (dst: [m]f32, vs: [n]f32, c: [n]f32) =
  let tmp = map2 (*) c vs
  in reduce_by_index (copy dst) (*) 1 is tmp

def main [n] [m] (is: [n]i64) (dst: [m]f32) (vs: [n]f32) (c: [n]f32) =
  vjp (f is) (dst, vs, c) (replicate m 1)
