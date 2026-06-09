-- Test with only variant 'xs'.
-- ==
-- input { [4i64,5i64,6i64] }
-- auto output

entry main [n] (xs : [n]i64) =
  map (\x -> reduce (+) 0 (replicate 6 x with [1:4] = iota 3)) xs
