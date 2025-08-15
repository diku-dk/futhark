-- ==
-- entry: fwd fwd_vec
-- input { [1.0f32, 2.0, 3.0] [4f32, 5, 6] }
-- output { [[1.0f32, 0.0, 0.0], [0.0f32, 2.0, 0.0], [0.0f32, 0.0, 3.0]] }

def prim = map2 (f32.*)

entry fwd [n] (xs: [n]f32) (ys: [n]f32) =
  tabulate n (\i -> jvp (prim xs) ys (replicate n 0 with [i] = 1))

entry fwd_vec [n] (xs: [n]f32) (ys: [n]f32) =
  let seeds = tabulate n (\i -> (replicate n 0 with [i] = 1))
  in jvp_vec (prim xs) ys seeds
