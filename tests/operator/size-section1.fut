-- Check that sizes are well calculated in right section

def main [n] [m] [l] (xs: [n]i64) (ys: [m]i64) (mat: [l][n]i64) =
  [xs ++ ys] ++ map (++ ys) mat
