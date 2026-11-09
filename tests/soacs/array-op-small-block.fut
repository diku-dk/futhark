-- Scan and segmented reduction with an array-typed operator, run with thread
-- blocks smaller than two in-block scan chunks (see the tuning file). The
-- block-level scan used by both must not need more memory than the operator
-- parameters provide, which is two elements per thread. This test is only
-- sensible on GPU.
-- ==
-- entry: scan_arr
-- random input { [33][2]i32 } auto output
-- random input { [100][10]i32 } auto output
-- compiled random input { [5000][2]i32 } auto output

-- ==
-- entry: segred_arr
-- random input { [64][2][4]i32 } auto output
-- random input { [40][7][2]i32 } auto output
-- random input { [100][5][3]i32 } auto output

def vecadd [n] (xs: [n]i32) (ys: [n]i32) : [n]i32 =
  loop res = #[scratch] replicate n 0
  for i < n do
    res with [i] = xs[i] + ys[i]

entry scan_arr [n] [m] (xss: [n][m]i32) =
  scan vecadd (replicate m 0) xss

entry segred_arr [k] [s] [m] (xsss: [k][s][m]i32) =
  map (reduce vecadd (replicate m 0)) xsss
