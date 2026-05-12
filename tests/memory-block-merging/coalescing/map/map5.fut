-- ==
-- compiled random input { [1024][1024]i64 }
-- auto output
-- structure gpu-mem { Alloc 3 }
-- structure seq-mem { Alloc 1 }
def main [m] [n] (xss: [m][n]i64) : [n][m]i64 =
  let xss' = transpose xss
  in map (\xs ->
            loop xs' = copy xs
            for i < m do
              let xs'[i] = xs'[i] * i
              in xs')
         xss'
