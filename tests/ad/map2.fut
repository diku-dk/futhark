-- Map with free variable.
-- ==
-- tags { autodiff }
-- entry: fwd_map rev_map rev_vec
-- input { 2.0f32 [1.0f32,2.0f32,3.0f32] }
-- output { [1.0f32,2.0f32,3.0f32] }

def primal xs (c': f32) = map (* c') xs

def onehot n i : [n]f32 =
  tabulate n (\j -> f32.bool (i == j))

entry fwd_map [n] (c: f32) (xs: [n]f32) =
  jvp (primal xs) c 1

entry rev_map [n] (c: f32) (xs: [n]f32) =
  tabulate n (\i -> vjp (primal xs) c (onehot n i))

entry rev_vec [n] (c: f32) (xs: [n]f32) =
  let seeds = tabulate n (\i -> onehot n i)
  in mjp (primal xs) c seeds
