-- ==
-- tags { autodiff }

-- ==
-- entry: fwd_map fwd_vec
-- input { 2i64 2i64 [1,2,3,4] }
-- output { [[[1, 0], [0, 0]], [[0, 1], [0, 0]]] }

entry fwd_map n m (xs: [n * m]i32) =
  tabulate 2 (\i -> jvp unflatten xs (replicate (n * m) 0 with [i] = 1))

entry fwd_vec n m (xs: [n * m]i32) =
  let seeds = tabulate 2 (\i -> replicate (n * m) 0 with [i] = 1)
  in jvp_vec unflatten xs seeds

-- ==
-- entry: f_vjp
-- input { 2i64 2i64 [1,2,3,4] }
-- output { [1,2,3,4] }

entry f_vjp n m (xs: [n * m]i32) =
  vjp unflatten xs (unflatten xs)
