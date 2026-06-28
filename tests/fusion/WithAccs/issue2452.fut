-- Regression test for issue #2452: map producing 2D arrays should fuse
-- into scatter (with_acc) via flatten. The unflattenAccOnlyMap optimisation
-- rewrites the flat scatter-map into a nested Screma(m, {Screma(n)}) so that
-- the outer Screma fuses with the upstream map(m) producer. The inner Screma(n)
-- remains, giving 2 Scremas total (outer row-loop + inner element-loop).
-- ==
-- structure { Screma 2 WithAcc 1 }

entry main [m][n] (xs: [m][n]i32) (dest: *[m*n]i32) : *[m*n]i32 =
  let (vals, inds) = unzip (map (\row -> (map (+1) row, map i64.i32 row)) xs)
  in scatter dest (flatten inds) (flatten vals)
