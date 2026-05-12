-- Check that sizes are well calculated in left section

def main [n] [m] [l] (xs: [n]i64) (ys: [m]i64) (mat: [l][m]i64) =
  [xs ++ ys] ++ map (xs ++) mat
