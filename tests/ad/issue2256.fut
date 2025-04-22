-- ==
-- entry: fwd rev
-- input { [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] }
-- output { [5040.0, 2521.0, 1684.0, 1278.0, 1104.0, 1440.0] }

def primal [m] (x: [m]f64) =
  let muls = scan (*) 1 x
  in f64.sum (map2 (*) muls x)

entry rev [m] (x: [m]f64) =
  vjp (\x' -> primal x') x 1

entry fwd [m] (x: [m]f64) =
  tabulate m (\i -> jvp (\x' -> primal x') x (replicate m 0 with [i] = 1))
