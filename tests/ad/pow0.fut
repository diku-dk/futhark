-- The power function has a dangerous kink for x==0.

-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 0.0 1.0 } output { 1.0 }
-- input { 0.0 0.0 } output { 0.0 }

-- ==
-- entry: rev
-- input { 0.0 1.0 } output { 1.0 0.0 }
-- input { 0.0 0.0 } output { 0.0 0.0 }

entry fwd x y : f64 = jvp (\(x, y) -> x ** y) (x, y) (1, 1)

entry rev x y = vjp (\(x, y) -> x ** y) (x, y) 1f64
