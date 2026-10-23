-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input { [4.0f32,3.0f32,2.0f32,1.0f32] [0i64,1i64,2i64,3i64] }
-- output { [[1.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [0.0f32, 1.0f32, 0.0f32, 0.0f32],
--           [0.0f32, 0.0f32, 1.0f32, 0.0f32],
--           [0.0f32, 0.0f32, 0.0f32, 1.0f32]]
--        }
-- input { [4.0f32,3.0f32,2.0f32,1.0f32] [0i64,0i64,3i64,3i64] }
-- output { [[1.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [1.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [0.0f32, 0.0f32, 0.0f32, 1.0f32],
--           [0.0f32, 0.0f32, 0.0f32, 1.0f32]]
--        }

def gather xs is = map (\(i: i64) -> xs[i]) is

entry fwd_J [n] [m] (xs: [n]f32) (is: [m]i64) =
  transpose (tabulate n (\j -> jvp (`gather` is) xs (replicate n 0 with [j] = 1)))

entry rev_J [n] [m] (xs: [n]f32) (is: [m]i64) =
  tabulate m (\j -> vjp (`gather` is) xs (replicate m 0 with [j] = 1))

entry fwd_vec_J [n] [m] (xs: [n]f32) (is: [m]i64) =
  let seeds = tabulate n (\j -> replicate n 0 with [j] = 1)
  in transpose (jmp (`gather` is) xs seeds)

entry rev_vec_J [n] [m] (xs: [n]f32) (is: [m]i64) =
  let seeds = tabulate m (\j -> replicate m 0 with [j] = 1)
  in mjp (`gather` is) xs seeds
