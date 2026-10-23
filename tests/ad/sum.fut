-- Simple reduce with summation.
-- ==
-- tags { autodiff }
-- entry: rev fwd fwd_vec
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32] }
-- output { [1.0f32, 1.0f32, 1.0f32, 1.0f32, 1.0f32, 1.0f32] }

def sum [n] (xs: [n]f32) =
  reduce (+) 0 xs

entry rev [n] (xs: [n]f32) =
  vjp sum xs 1

entry fwd [n] (xs: [n]f32) =
  tabulate n (\i -> jvp sum xs (tabulate n ((== i) >-> f32.bool)))

entry fwd_vec [n] (xs: [n]f32) =
  let seeds = tabulate n (\i -> tabulate n ((== i) >-> f32.bool))
  in jmp sum xs seeds
