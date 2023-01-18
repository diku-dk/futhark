-- Test with only variant 'xs'.
-- ==
-- input { [4i64,5i64,6i64] }
-- output { [3i64,7i64,12i64] }

entry main [n] (xs : [n]i64) =
  map (\x -> reduce (+) 0 (iota x with [1:4] = iota 3)) xs
