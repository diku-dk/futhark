-- Simple test for flattening an update with a constant value
-- ==
-- input { [1i64,2i64,3i64] }
-- output { [12i64,11i64,10i64] }
-- structure gpu { /SegRed 1 /SegMap/Update 1 }

entry main [n] (xs: [n]i64) =
  #[flattening(only_inner)]
  map (\x -> reduce (+) 0 (iota 5 with [x] = 3)) xs
