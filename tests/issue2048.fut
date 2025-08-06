-- ==
-- input { [1,2,3] }
-- output { 3i64 }

def f [n] (xs: [n]i32) =
  let [m] (ys: [m]i32) = filter (> 0) xs
  in ys

entry main xs = length (f xs)
