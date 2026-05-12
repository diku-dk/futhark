-- Check that sizes are well calculated in left section, with complex sizes
-- ==
def main [n] [m] [l] (xs: [n]i64) (ys: [m]i64) (mat: [l][m]i64) =
  [(xs ++ xs) ++ ys] ++ map (xs ++ xs ++) mat
