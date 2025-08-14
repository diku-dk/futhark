-- ==
-- entry: rev
-- input { [5f32,1f32,2f32,3f32,4f32] [0i64,0i64,0i64,0i64,0i64] [4f32,3f32,2f32,1f32,0f32] } output { [0f32,0f32,0f32,0f32,0f32] }
-- input { [4f32,1f32,2f32,3f32,4f32] [0i64,0i64,0i64,0i64,0i64] [5f32,4f32,3f32,2f32,1f32] } output { [1f32,0f32,0f32,0f32,0f32] }
-- input { [5f32,1f32,2f32,3f32,4f32] [0i64,0i64,0i64,0i64,0i64] [5f32,4f32,3f32,2f32,1f32] } output { [0f32,0f32,0f32,0f32,0f32] }
def red_max [n] [m] (dst: [m]f32) (is: [n]i64) (vs: [n]f32) =
  reduce_by_index (copy dst) f32.max f32.lowest is vs

entry rev [n] [m] (dst: [m]f32) (is: [n]i64) (vs: [n]f32) =
  vjp (red_max dst is) vs (replicate m 0 with [0] = 1)
