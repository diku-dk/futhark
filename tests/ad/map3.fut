-- ==
-- tags { autodiff }
-- entry: fwd rev_map rev_vec
-- input { 1i32 [1i32,2i32,3i32] }
-- output { [1i32,2i32,3i32] }

def primal xs (x: i32) = map (* x) xs

entry fwd [n] (x: i32) (xs: [n]i32) =
  jvp (primal xs) x 1

entry rev_map [n] (x: i32) (xs: [n]i32) =
  tabulate n (\i ->
                vjp (primal xs) x (replicate n 0 with [i] = 1))

entry rev_vec [n] (x: i32) (xs: [n]i32) =
  let seeds = tabulate n (\i -> (replicate n 0 with [i] = 1))
  in vjp_vec (primal xs) x seeds
