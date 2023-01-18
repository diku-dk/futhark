-- Test with fixed dimension
-- ==
-- input { [0,1,2,3,4] [5i64,6i64,7i64,8i64,9i64] }
-- output { [[5i64,0i64,0i64,0i64,0i64]
--          ,[1i64,6i64,1i64,1i64,1i64]
--          ,[2i64,2i64,7i64,2i64,2i64]
--          ,[3i64,3i64,3i64,8i64,3i64]
--          ,[4i64,4i64,4i64,4i64,9i64]] }

let main is js =
  [let n = 0 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 1 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 2 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 3 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 4 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ]
