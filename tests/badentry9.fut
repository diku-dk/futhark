-- Entry points must use all sizes constructively.
-- ==
-- error: \[x\].*constructive

entry main [x] (_: [x + 1]i32) = x
