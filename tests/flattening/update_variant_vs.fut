-- Test with only variant 'vs'.
-- ==
-- input { [3i64,3i64,3i64] }
-- output { [7i64,7i64,7i64] }

entry main (vs : []i64) =
  map (\v -> reduce (+) 0 (iota 5 with [1:4] = iota v)) vs
