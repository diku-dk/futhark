-- A funky scan with vectorised operator (and not interchangeable).
-- ==
-- compiled random input { [1][100]i32 } auto output
-- compiled random input { [100][1]i32 } auto output
-- compiled random input { [100][100]i32 } auto output

def main [n] [m] (xss: [n][m]i32) =
  scan (map2 (+)) (replicate m 0) (map (scan (+) 0) xss)
