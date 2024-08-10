-- ==
-- entry: fwd
-- input { 1.0 }
-- output { 1f32 }

entry fwd x =
  jvp f32.f64 x 1

-- ==
-- entry: rev
-- input { 1.0 }
-- output { 1f64 }

entry rev x =
  vjp f32.f64 x 1
