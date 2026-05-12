-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 1f64 }
-- output { 1i32 }

entry fwd x =
  jvp i32.f64 x 1

-- ==
-- entry: rev
-- input { 1f64 }
-- output { 2f64 }

entry rev x =
  vjp i32.f64 x 2
