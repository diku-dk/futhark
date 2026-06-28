-- Scan with vectorised addition.
-- special cases: vectorised and addition
-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input { [[1f32,1f32],[2f32,2f32],[3f32,3f32],[4f32,4f32],[5f32,5f32]] }
-- output { [[[1.000000f32, 1.000000f32], [0.000000f32, 0.000000f32], [0.000000f32, 0.000000f32], [0.000000f32, 0.000000f32], [0.000000f32, 0.000000f32]], [[1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [0.000000f32, 0.000000f32], [0.000000f32, 0.000000f32], [0.000000f32, 0.000000f32]], [[1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [0.000000f32, 0.000000f32], [0.000000f32, 0.000000f32]], [[1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [0.000000f32, 0.000000f32]], [[1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32], [1.000000f32, 1.000000f32]]] }

def primal [n] [k] (a: [n][k]f32) =
  scan (map2 (+)) (replicate k 0) a

entry fwd_J [n] [k] (a: [n][k]f32) =
  tabulate n (\i -> jvp primal a (replicate n (replicate k 0) with [i] = replicate k 1))
  |> transpose

entry rev_J [n] [k] (a: [n][k]f32) =
  tabulate n (\i -> vjp primal a (replicate n (replicate k 0) with [i] = replicate k 1))

entry fwd_vec_J [n] [k] (a: [n][k]f32) =
  let seeds = tabulate n (\i -> replicate n (replicate k 0) with [i] = replicate k 1)
  in jmp primal a seeds |> transpose

entry rev_vec_J [n] [k] (a: [n][k]f32) =
  let seeds = tabulate n (\i -> replicate n (replicate k 0) with [i] = replicate k 1)
  in mjp primal a seeds
