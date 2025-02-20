-- Test with only invariant indices.
-- ==
-- input { [4i64,5i64,6i64] [3i64,3i64,3i64] }
-- output { [3i64,7i64,12i64] }

entry main [n] (xs : [n]i64) (vs : [n]i64) =
  map2(\x v -> reduce (+) 0 (iota x with [1:4] = iota v)) xs vs
