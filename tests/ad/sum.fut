-- Simple reduce with summation.
-- ==
-- tags { autodiff }
-- entry: rev fwd
-- input { [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] }
-- output { [1.0, 1.0, 1.0, 1.0, 1.0, 1.0] }

def sum [n] (xs: [n]f64) =
  reduce (+) 0 xs

entry rev [n] (xs: [n]f64) =
  vjp sum xs 1

entry fwd [n] (xs: [n]f64) =
  tabulate n (\i -> jvp sum xs (tabulate n ((== i) >-> f64.bool)))
