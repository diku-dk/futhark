-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 2f32 1f32 }
-- output { 1065353216i32 }

-- ==
-- entry: rev
-- input { 2f32 0i32 }
-- output { 1.0e-45f32 }

entry fwd = jvp (\x -> i32.u32 (f32.to_bits x))

entry rev = vjp (\x -> i32.u32 (f32.to_bits x))
