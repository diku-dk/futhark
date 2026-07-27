-- Test with only variant 'vs'.
-- ==
-- input { [3i64,3i64,3i64] }
-- output { [7i64,7i64,7i64] }
-- structure gpu { /WithAcc 1 /Apply/segiota 1 /Apply/repiota 1 Update 0 }

entry main (vs: []i64) =
  #[incremental_flattening(only_inner)]
  map (\v -> reduce (+) 0 (iota 5 with [1:4] = iota v)) vs
