-- ==
-- entry: rev fwd
-- input { [1.0, 2.0, 3.0, 4.0, 5.0, -6.0, 5.0] }
-- output { [0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0] }
-- input { [1.0, 1.0] }
-- output { [1.0, 0.0] }
-- structure { /Screma 2 }

def f = map f64.abs >-> f64.maximum

entry rev [n] (xs: [n]f64) =
  vjp f xs 1

entry fwd [n] (xs: [n]f64) =
  tabulate n (\i -> jvp f xs (tabulate n ((== i) >-> f64.bool)))
