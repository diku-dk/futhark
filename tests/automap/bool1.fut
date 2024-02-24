-- ==
-- entry: f
-- input { [true, true, false] [false, true, true] }
-- output { [true, true, true] }

def f [m] (xs: [m]bool) (ys: [m]bool) = xs || ys
