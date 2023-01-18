-- Test with only invariant 'vs'.
-- ==
-- input { [6i64,7i64,8i64] [0i64,1i64,2i64] [5i64,6i64,7i64] }
-- output { [15i64,16i64,18i64] }

entry main [n] (xs : [n]i64) (is : [n]i64) (js : [n]i64) =
  map3(\x i j -> reduce (+) 0 (iota x with [i:j] = iota 5)) xs is js

