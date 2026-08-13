-- Test with only variant 'xs'.
-- ==
-- input { [4i64,5i64,6i64] }
-- output { [3i64,7i64,12i64] }
-- structure gpu { /WithAcc 1 /Apply/segiota 2 /Apply/repiota 1 Update 0 }

entry main [n] (xs: [n]i64) =
  #[flattening(only_inner)]
  map (\x -> reduce (+) 0 (iota x with [1:4] = iota 3)) xs
