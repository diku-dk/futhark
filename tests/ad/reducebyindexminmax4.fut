-- ==
-- entry: rev
-- input  { [5f32,1f32,2f32] [0i64,0i64,0i64] [4f32,3f32,2f32] 3f32 }
--          output { [3f32,0f32,0f32] 5f32 }
-- input  { [5f32,1f32,2f32] [0i64,0i64,0i64] [10f32,3f32,2f32] 3f32 }
--          output { [0f32,0f32,0f32] 10f32 }
-- input  { [5f32,1f32,2f32] [0i64,1i64,0i64] [10f32,30f32,2f32] 3f32 }
--          output { [0f32,0f32,0f32] 10f32 }
def red_max [n][m] (vs: [n]f32) (is: [n]i64) (dst: [m]f32, c: f32) =
  let red = reduce_by_index (copy dst) f32.max f32.lowest is vs
  in map (*c) red

entry rev [n][m] (dst: [m]f32) (is: [n]i64) (vs: [n]f32) (c: f32) =
  vjp (red_max vs is) (dst,c) (replicate m 0 with [0] = 1)
  --tabulate n (\i -> vjp (red_max dst is) (vs, c) (replicate n 0 with [i] = 1))