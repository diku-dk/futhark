-- Simple test for flattening an update with a constant value
-- ==
-- input { [1i64,2i64,3i64] }
-- output { [12i64,11i64,10i64] }

entry main [n] (xs : [n]i64) =
  map (\x -> reduce (+) 0 (iota 5 with [x] = 3)) xs
