-- Even more funky scan with vectorised operator on arrays (and not interchangeable).
-- ==
-- compiled random input { [1][10][100]i32 } auto output
-- compiled random input { [100][10][1]i32 } auto output
-- compiled random input { [100][10][100]i32 } auto output

def vecadd [n] (xs: [n]i32) (ys: [n]i32) : [n]i32 =
  loop res = replicate n 0
  for i < n do
    res with [i] = xs[i] + ys[i]

def main [n] [m] [k] (xsss: [n][m][k]i32) =
  scan (map2 vecadd) (replicate m (replicate k 0)) (map (scan (map2 (+)) (replicate k 0)) xsss)
