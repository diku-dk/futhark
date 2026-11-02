-- Intra-block scan with an array-typed operator.
-- ==
-- random input { [2][4][2]i32 } auto output
-- random input { [2][100][2]i32 } auto output
-- random input { [2][1000][2]i32 } auto output
-- structure gpu { SegMap/SegScan 1 }

def op [m] (xs: [m]i32) (ys: [m]i32) : [m]i32 =
  loop zs = replicate m 0i32
  for i < m do
    let zs[i] = xs[i] + ys[i]
    in zs

def main xsss =
  #[flattening(only_intra)]
  map (scan op (replicate 2 0i32)) xsss
