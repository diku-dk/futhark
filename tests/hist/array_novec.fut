-- Test reduce_by_index on array of arrays, where the operator is not
-- recognisably vectorised.
-- ==
-- input { [[1,2,3],[4,5,6]] [0i64,0i64,2i64,1i64] }
-- output { [[1, 4, 7], [4, 6, 8]] }

def main [m][n][k] (xs : *[n][m]i32) (image : *[k]i64) : *[n][m]i32 =
  reduce_by_index xs (\x y -> loop acc = copy x for i in iota m do acc with [i] = acc[i] + y[i])
                  (replicate m 0)
                  image (replicate k (map i32.i64 (iota m)))
