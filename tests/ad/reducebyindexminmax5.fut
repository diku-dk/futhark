-- ==
-- entry: rev
-- input  { [4f32,1f32,2f32] [0i64,0i64,0i64] [5f32,1f32,2f32]}
--          output { [13f32,5f32,5f32] }
def red_max [n] [m] (dst: [m]f32) (is: [n]i64) (vs: [n]f32) =
  let red = reduce_by_index (copy dst) f32.max f32.lowest is vs
  in map (* reduce (+) 0 vs) red

entry rev [n] [m] (dst: [m]f32) (is: [n]i64) (vs: [n]f32) =
  vjp (red_max dst is) vs (replicate m 0 with [0] = 1)
