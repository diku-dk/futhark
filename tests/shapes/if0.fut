-- Inferring an invariant size for a branch.
-- ==
-- input { [1,2,3] [2,3,4] }

let main [n] (xs: [n]i32) (ys: *[n]i32) : [n]i32 =
  if true
  then xs
  else loop ys for i < n do ys with [i] = ys[i] + 1
