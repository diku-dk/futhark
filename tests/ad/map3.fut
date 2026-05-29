-- ==
-- tags { autodiff }
-- entry: fwd rev rev_vec
-- input { 1i32 [1i32,2i32,3i32] }
-- output { [1i32,2i32,3i32] }

entry fwd [n] (x: i32) (xs: [n]i32) =
  jvp (\x -> map (* x) xs) x 1

entry rev [n] (x: i32) (xs: [n]i32) =
  tabulate n (\i ->
                vjp (\x -> map (* x) xs) x (replicate n 0 with [i] = 1))

entry rev_vec [n] (x: i32) (xs: [n]i32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in vjp_vec (\x -> map (* x) xs) x seeds
