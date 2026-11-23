-- Test that we safely hoist a potentially unsafe operation out of a lop.
-- ==
-- entry: rotate
-- input { 1i64 empty([0]i32) }
-- output { empty([0]i32) }
-- structure { /Assert 1 Assert 2 }

entry rotate [n] (r: i64) (xs: [n]i32) =
  map (\i -> xs[(i + r) % n]) (iota n)
