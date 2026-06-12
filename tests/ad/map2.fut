-- Map with free variable.
-- ==
-- tags { autodiff }
-- entry: fwd_map rev_map rev_vec
-- input { 2.0 [1.0,2.0,3.0] }
-- output { [1.0,2.0,3.0] }

def primal xs (c': f64) = map (* c') xs

def onehot n i : [n]f64 =
  tabulate n (\j -> f64.bool (i == j))

entry fwd_map [n] (c: f64) (xs: [n]f64) =
  jvp (primal xs) c 1

entry rev_map [n] (c: f64) (xs: [n]f64) =
  tabulate n (\i -> vjp (primal xs) c (onehot n i))

entry rev_vec [n] (c: f64) (xs: [n]f64) =
  let seeds = tabulate n (\i -> onehot n i)
  in vjp_vec (primal xs) c seeds
