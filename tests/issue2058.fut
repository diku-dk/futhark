-- We neglected to mark the target arrays as consumed while
-- simplifying the body.
-- ==
-- tags { no_webgpu }

entry problem [n] (arr: *[n]i64) : [n]i64 =
  reduce_by_index arr (+) 0 (iota n) (copy arr)
