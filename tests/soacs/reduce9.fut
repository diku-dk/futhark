-- ==
-- compiled random input { [512][32]i32 } auto output
-- compiled random input { [1024][1024]i32 } auto output

def main [n][m] (xss: [n][m]i32): []i32 =
  reduce (\(xs: []i32) ys ->
               loop zs = replicate m 0 for i < m do
                 let zs[i] = xs[i] + ys[i]
                 in zs
         )
         (replicate m 0)
         xss
