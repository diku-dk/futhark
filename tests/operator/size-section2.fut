-- Check that sizes are well calculated in left section, even with bounded existential sizes

def main [n] [m] [l] (xs: [n]i64) (ys: [m]i64) (mat: [l][m]i64) =
  let xs' = filter (> 0) xs
  in [xs' ++ ys] ++ map (xs' ++) mat
