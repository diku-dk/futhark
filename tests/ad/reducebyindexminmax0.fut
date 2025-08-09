-- ==
-- entry: rev
-- input { [0i64, 1i64, 2i64, 3i64, 4i64] [0.0f32, 1.0f32, 2.0f32, 3.0f32, 4.0f32] } output { [0f32,0f32,0f32,0f32,0f32] }
-- input { [0i64, 0i64, 0i64, 0i64, 0i64] [0.0f32, 1.0f32, 2.0f32, 3.0f32, 4.0f32] } output { [0f32,0f32,0f32,0f32,1f32] }
-- input { [0i64, 0i64, 0i64, 0i64, 0i64] [0.0f32, 1.0f32, 2.0f32, 3.0f32, 3.0f32] } output { [0f32,0f32,0f32,1f32,0f32] }

-- ==
-- entry: revp
-- input { [0i64, 1i64, 2i64, 3i64, 4i64] [0.0f32, 1.0f32, 2.0f32, 3.0f32, 4.0f32] } output { [0i64,0i64,0i64,0i64,0i64] }
-- input { [0i64, 0i64, 0i64, 0i64, 0i64] [0.0f32, 1.0f32, 2.0f32, 3.0f32, 4.0f32] } output { [0i64,0i64,0i64,0i64,0i64] }

def red_max [n] (is: [n]i64, vs: [n]f32) =
  reduce_by_index (replicate 5 0) f32.max f32.lowest is vs

entry rev [n] (is: [n]i64) (vs: [n]f32) =
  let (_, res) = vjp red_max (is, vs) (replicate 5 0 with [0] = 1)
  in res

entry revp [n] (is: [n]i64) (vs: [n]f32) =
  let (res, _) = vjp red_max (is, vs) (replicate 5 0 with [0] = 1)
  in res
