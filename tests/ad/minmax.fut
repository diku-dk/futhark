-- ==
-- entry: rev fwd
-- input { [1.0, 2.0, 3.0, 4.0, 5.0, -6.0, 5.0] }
-- output { [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
--          [0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0]
--        }
-- structure { /Screma 2 }

def f xs = let ys = map f64.abs xs
           in (f64.minimum ys, f64.maximum ys)

entry rev [n] (xs: [n]f64) =
  (vjp f xs (1,0),
   vjp f xs (0,1))

entry fwd [n] (xs: [n]f64) =
  unzip (tabulate n (\i -> jvp f xs (tabulate n ((==i) >-> f64.bool))))
