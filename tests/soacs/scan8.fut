-- Segmented scan with array operator (interchangeable).
-- ==
-- random input { [10][1][10]i32 } auto output
-- random input { [10][10][1]i32 } auto output
-- random input { [10][10][10]i32 } auto output
-- structure gpu { /SegScan 1 /SegScan/Loop 0 }

def main [n] [m] [k] (xss: [n][m][k]i32) =
  map (scan (map2 (+)) (replicate k 0)) xss
