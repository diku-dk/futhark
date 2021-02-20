-- ==
-- entry: fwd
-- compiled input { 1.0 2.0 }
-- output { false false }
-- compiled input { 1.0 1.0 }
-- output { true true }

entry fwd x y =
  (jvp (\(a, b) -> f64.(a == b)) (x,y) (1,0),
   jvp (\(a, b) -> f64.(a == b)) (x,y) (0,1))

-- ==
-- entry: rev
-- compiled input { 1.0 2.0 }
-- output { 1.0 1.0 }
-- compiled input { 1.0 1.0 }
-- output { 1.0 1.0 }

entry rev x y =
  vjp (\(a, b) -> f64.(a == b)) (x,y) true
