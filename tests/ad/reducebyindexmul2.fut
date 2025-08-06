-- ==
-- entry: rev
-- input { [2f32,1f32,1f32,1f32,1f32] [0i64,0i64,0i64,0i64,0i64] [1f32,2f32,3f32,4f32,10f32] } output { [244f32,0f32,0f32,0f32,0f32] }
-- input { [10f32,1f32,1f32,1f32,1f32] [0i64,0i64,0i64,0i64,0i64] [0f32,2f32,3f32,4f32,5f32] } output { [20f32,0f32,0f32,0f32,0f32] }
-- input { [0f32,1f32,1f32,1f32,1f32] [0i64,0i64,0i64,0i64,0i64] [1f32,2f32,3f32,4f32,5f32] } output { [120f32,0f32,0f32,0f32,0f32] }
-- input { [0f32,1f32,1f32,1f32,1f32] [0i64,0i64,0i64,0i64,0i64] [1f32,2f32,0f32,4f32,5f32] } output { [0f32,0f32,0f32,0f32,0f32] }

-- checks original dst is used
def red_mul [n] [m] (is: [n]i64) (vs: [n]f32) (dst: [m]f32) =
  let dst2 = copy dst
  let a = map (** 2) dst2
  let b = reduce_by_index dst2 (*) 1 is vs
  in map2 (+) a b

entry rev [n] [m] (dst: [m]f32) (is: [n]i64) (vs: [n]f32) =
  vjp (red_mul is vs) dst (replicate m 0 with [0] = 1)
