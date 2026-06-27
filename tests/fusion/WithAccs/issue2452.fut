-- Regression test for issue #2452: map producing 2D arrays should fuse
-- into scatter (with_acc) via flatten. Before the fix, the scatter rewrite
-- in terms of accumulators broke this fusion, leaving an extra Screma.
-- ==
-- structure { Screma 1 WithAcc 1 }

entry main [m][n] (xs: [m][n]i32) (dest: *[m*n]i32) : *[m*n]i32 =
  let (vals, inds) = unzip (map (\row -> (map (+1) row, map i64.i32 row)) xs)
  in scatter dest (flatten inds) (flatten vals)
