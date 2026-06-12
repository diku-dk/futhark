-- Reduce with vectorised addition.
-- ==
-- tags { autodiff }
-- entry: fwd_map fwd_vec rev_vec
-- input { [[1f32,1f32],[2f32,2f32],[3f32,3f32],[4f32,4f32],[5f32,5f32]] }
-- output { [[1.0f32, 1.0], [1.0f32, 1.0], [1.0f32, 1.0], [1.0f32, 1.0], [1.0f32, 1.0]] }

def primal [n] [k] (a: [n][k]f32) =
  reduce (map2 (+)) (replicate k 0) a

entry fwd_map [n] [k] (a: [n][k]f32) =
  tabulate n (\i -> jvp primal a (replicate n (replicate k 0) with [i] = replicate k 1))

entry fwd_vec [n] [k] (a: [n][k]f32) =
  jvp_vec primal a (tabulate n (\i -> (replicate n (replicate k 0) with [i] = replicate k 1)))

entry rev_vec [n] [k] (a: [n][k]f32) =
  head (vjp_vec primal a [replicate k 1])
