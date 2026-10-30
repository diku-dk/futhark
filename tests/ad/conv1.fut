-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 1f32 }
-- output { 1i32 }

entry fwd x =
  jvp i32.f32 x 1

-- ==
-- entry: rev
-- input { 1f32 }
-- output { 2f32 }

entry rev x =
  vjp i32.f32 x 2
