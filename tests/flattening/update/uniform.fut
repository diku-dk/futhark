-- Test with only variant 'xs'.
-- ==
-- input { [4i64,5i64,6i64] }
-- auto output
-- structure gpu { WithAcc 0 Update 1 }

entry main [n] (xs: [n]i64) =
  #[incremental_flattening(only_inner)]
  map (\x -> reduce (+) 0 (replicate 6 x with [1:4] = iota 3)) xs
