-- Map with free variable.
-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J
-- input { 2.0 [1.0,2.0,3.0] }
-- output { [1.0,2.0,3.0] }

def onehot n i : [n]f64 =
  tabulate n (\j -> f64.bool (i == j))

entry fwd_J [n] (c: f64) (xs: [n]f64) =
  jvp (\c' -> map (* c') xs) c 1

entry rev_J [n] (c: f64) (xs: [n]f64) =
  tabulate n (\i -> vjp (\c' -> map (* c') xs) c (onehot n i))
