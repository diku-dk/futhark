-- Check that sizes are well calculated in right section, with complex sizes
-- ==
def main [n] [m] [l] (xs: [n]i64) (ys: [m]i64) (mat: [l][m]i64) =
  [ys ++ (xs ++ xs)] ++ map (++ xs ++ xs) mat
