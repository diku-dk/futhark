-- ==
-- input {
--  [0i64,1i64,0i64,1i64]
--  [[1f32,2f32],[3f32,4f32]]
--  [[1f32,0f32],[5f32,2f32],[-2f32,3f32],[4f32,6f32]]
--  [[1f32,2f32],[3f32,4f32]]
--  [[3f32,4f32],[5f32,6f32],[4f32,5f32],[6f32,7f32]]
-- }
-- output { [[3f32,4f32],[8f32,6f32],[4f32,7f32],[6f32,11f32]] }

def primal3 [n][m][k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  (reduce_by_index (copy dst) (map2 f32.max) (replicate k f32.lowest) is vs,vs)

def main [n][m][k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) bar1 bar2 =
  vjp (primal3 is dst) vs (bar1,bar2)
