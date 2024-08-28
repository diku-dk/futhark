-- Segmented scan with array operator (and not interchangeable).
-- ==
-- random input { [10][1][10]i32 } auto output
-- random input { [10][10][1]i32 } auto output
-- random input { [10][10][10]i32 } auto output
-- structure gpu { /SegScan 1 /SegScan/SegBinOp/Loop 1 }

def vecadd [m] (xs: [m]i32) (ys: [m]i32): [m]i32 =
  loop xs = copy xs for i < m do
    let xs[i] = xs[i] + ys[i]
    in xs

def main [n][m][k] (xss: [n][m][k]i32) =
  map (scan vecadd (replicate k 0)) xss
