-- ==
-- tags { autodiff }
-- entry: fwd rev
-- input { 1i32 [1i32,2i32,3i32] }
-- output { [1i32,2i32,3i32] }

entry fwd [n] (x: i32) (xs: [n]i32) =
  jvp (\x -> map (* x) xs) x 1

entry rev [n] (x: i32) (xs: [n]i32) =
  tabulate n (\i ->
                vjp (\x -> map (* x) xs) x (replicate n 0 with [i] = 1))
