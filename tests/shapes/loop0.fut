-- Inferring an invariant size for a loop.
-- ==
-- input { [1,2,3] }
-- input { [2,3,4] }

let main [n] (xs: *[n]i32) : [n]i32 =
  loop xs for i < n do xs with [i] = xs[i] + 1
