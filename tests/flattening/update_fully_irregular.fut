-- Fully irregular test-case
-- ==
-- input { [5i64,4i64,3i64] [1i64,2i64,3i64] [3i64,3i64,3i64] }
-- output { [8i64,4i64,3i64] }

entry main [n] (xs : [n]i64) (is : [n]i64) (js : [n]i64) =
  map3 (\x i j -> reduce (+) 0 (iota x with [i:j] = iota (j-i))) xs is js
