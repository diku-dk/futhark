-- ==
-- tags { autodiff no_webgpu }
-- entry: rev fwd fwd_vec
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, -6.0f32, 5.0f32] }
-- output { [0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, -1.0f32, 0.0f32] }
-- input { [1.0f32, 1.0f32] }
-- output { [1.0f32, 0.0f32] }
-- structure { /Screma 3 }

def f = map f32.abs >-> f32.maximum

entry rev [n] (xs: [n]f32) =
  vjp f xs 1

entry fwd [n] (xs: [n]f32) =
  tabulate n (\i -> jvp f xs (tabulate n ((== i) >-> f32.bool)))

entry fwd_vec [n] (xs: [n]f32) =
  let seeds = tabulate n (\i -> tabulate n ((== i) >-> f32.bool))
  in jmp f xs seeds
