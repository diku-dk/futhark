-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 2f64 }
-- output { 0i32 }

entry fwd x =
  jvp (\x -> i32.f64 (f64.ceil x)) x 1

-- ==
-- entry: rev
-- input { 2f64 }
-- output { 0f64 }

entry rev x =
  vjp (\x -> i32.f64 (f64.ceil x)) x 2
