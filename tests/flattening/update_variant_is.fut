-- Test with only variant indices.
-- ==
-- input { [0i64,3i64,1i64] [5i64,8i64,6i64] }
-- output { [28i64,13i64,23i64] }

entry main [n] (is : [n]i64) (js : [n]i64) =
  map2 (\i j -> reduce (+) 0 (iota 8 with [i:j] = iota 5)) is js
