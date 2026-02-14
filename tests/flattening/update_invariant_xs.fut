-- Test with only invariant 'xs'.
-- ==
-- input { [1i64,2i64,3i64] [3i64,3i64,3i64] }
-- output { [8i64,8i64,10i64] }

entry main [n] (is : [n]i64) (js : [n]i64) =
  map2(\i j -> reduce (+) 0 (iota 5 with [i:j] = iota (j-i))) is js
