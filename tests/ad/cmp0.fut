-- The derivatives of comparisons are always zero, but we should at
-- least not crash.

-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 1.0 2.0 }
-- output { false false }
-- input { 1.0 1.0 }
-- output { false false }

entry fwd x y =
  ( jvp (\(a, b) -> f64.(a == b)) (x, y) (1, 0)
  , jvp (\(a, b) -> f64.(a == b)) (x, y) (0, 1)
  )

-- ==
-- entry: rev
-- input { 1.0 2.0 }
-- output { 0.0 0.0 }
-- input { 1.0 1.0 }
-- output { 0.0 0.0 }

entry rev x y =
  vjp (\(a, b) -> f64.(a == b)) (x, y) true
