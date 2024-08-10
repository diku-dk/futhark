-- 0 zero - dst neutral / dst no neutral
-- 1 zero - dst / vs
-- 2 zero - both in vs / one in dst and one in vs
-- bucket with no values - zero / not zero
-- index out of bounds

-- ==
--  entry: main
--  input { 
--    [4i64,5i64,2i64,4i64,5i64,2i64,0i64,0i64,4i64,5i64,1i64,4i64,1i64,3i64,3i64,1i64,8i64,-1i64]
--    [1f32,2f32,0f32,4f32,5f32,0f32,9f32,0f32]
--    [11f32,16f32,7f32,0f32,14f32,6f32,2f32,1f32,13f32,0f32,5f32,0f32,3f32,0f32,9f32,4f32,17f32,18f32] }
--  output { 
--    [2f32,60f32,42f32,0f32,0f32,0f32,1f32,1f32]
--    [0f32,0f32,0f32,0f32,0f32,0f32,1f32,2f32,0f32,0f32,24f32,0f32,40f32,36f32,0f32,30f32,0f32,0f32] }
def f [n][m] (is: [n]i64) (dst: [m]f32,vs: [n]f32) =
  reduce_by_index (copy dst) (*) 1 is vs

def main [n][m] (is: [n]i64) (dst: [m]f32) (vs: [n]f32) =
  vjp (f is) (dst,vs) (replicate m 1)

--    [0i64,0i64,1i64,1i64,1i64,2i64,2i64,3i64,3i64,4i64,4i64,4i64,4i64,5i64,5i64,5i64]
--    [1f32,2f32,3f32,4f32,5f32,6f32,7f32,0f32,9f32,0f32,11f32,0f32,13f32,14f32,0f32,16f32]
--    [2f32,1f32,40f32,30f32,24f32,0f32,0f32,36f32,0f32,0f32,0f32,0f32,0f32,0f32,0f32,0f32]