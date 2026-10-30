-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 2f32 }
-- output { 0i32 }

entry fwd x =
  jvp (\x -> i32.f32 (f32.ceil x)) x 1

-- ==
-- entry: rev
-- input { 2f32 }
-- output { 0f32 }

entry rev x =
  vjp (\x -> i32.f32 (f32.ceil x)) x 2
