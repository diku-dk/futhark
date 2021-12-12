-- Test reduce_by_index on array of arrays
-- ==

def main [m][n] (xs : *[n][m]i32) (image : *[n]i64) : *[n][m]i32 =
  reduce_by_index xs (\x y -> map2 (+) x y) (replicate m 0)
                  image (replicate n (map i32.i64 (iota m)))
