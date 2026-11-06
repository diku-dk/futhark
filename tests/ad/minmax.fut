-- ==
-- tags { autodiff no_webgpu }
-- entry: rev fwd fwd_vec
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, -6.0f32, 5.0f32] }
-- output { [1.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32]
--          [0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, -1.0f32, 0.0f32]
--        }
-- structure { /Screma 3 }

def f xs =
  let ys = map f32.abs xs
  in (f32.minimum ys, f32.maximum ys)

entry rev [n] (xs: [n]f32) =
  ( vjp f xs (1, 0)
  , vjp f xs (0, 1)
  )

entry fwd [n] (xs: [n]f32) =
  unzip (tabulate n (\i -> jvp f xs (tabulate n ((== i) >-> f32.bool))))

entry fwd_vec [n] (xs: [n]f32) =
  let seeds = tabulate n (\i -> tabulate n ((== i) >-> f32.bool))
  in unzip (jmp f xs seeds)
