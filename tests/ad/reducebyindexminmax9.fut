-- ==
-- tags { autodiff }
-- input {
--  [0i64,1i64,0i64,1i64]
--  [[[1f32,2f32],[3f32,4f32]],[[5f32,6f32],[7f32,8f32]]]
--  [ [[1f32,0f32],[5f32,2f32]], [[7f32,4f32],[9f32,7f32]], [[-2f32,3f32],[4f32,6f32]], [[1f32,2f32],[5f32,9f32]] ]
--  [[[1f32,2f32],[3f32,4f32]],[[5f32,6f32],[7f32,8f32]]]
--  [ [[3f32,4f32],[5f32,6f32]], [[7f32,8f32],[9f32,10f32]], [[4f32,5f32],[6f32,7f32]], [[8f32,9f32],[10f32,11f32]] ]
-- }
-- output { [ [[3f32,4f32], [8f32,6f32]], [[12f32,8f32], [16f32,10f32]], [[4f32,7f32], [6f32,11f32]], [[8f32,9f32],[10f32,19f32]] ] }

def primal4 [n] [m] [k] [l] (is: [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  (reduce_by_index (copy dst) (map2 (map2 f32.max)) (replicate k (replicate l f32.lowest)) is vs, vs)

def main [n] [m] [k] [l] (is: [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) bar1 bar2 =
  vjp (primal4 is dst) vs (bar1, bar2)
