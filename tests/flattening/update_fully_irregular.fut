-- Fully irregular test-case
-- ==
-- input { [5i64,6i64,7i64] [2i64,3i64,1i64] [3i64,1i64,2i64] [5i64,6i64,3i64] [1i64,2i64,3i64] }
-- output { [4i64,9i64,19i64] }
-- structure gpu { /WithAcc 4 Update 0 }

entry main [n] (xs: [n]i64) (vs: [n]i64) (is: [n]i64) (js: [n]i64) (ss: [n]i64) =
  #[incremental_flattening(only_inner)]
  map5 (\x v i j s -> reduce (+) 0 (iota x with [i:j:s] = iota v)) xs vs is js ss
