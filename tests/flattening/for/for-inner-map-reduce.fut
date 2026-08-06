-- ==
-- input { [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] }
-- auto output
-- structure gpu { /Loop/ForLoop 1 Loop 1 /Loop/SegRed 1 /Loop/SegMap 1 }

def main [n] [m] (xss: [n][m]i64) =
  #[incremental_flattening(only_inner)]
  map (\xs ->
         let d =
           loop (ys, s) = (xs, 0)
           for i < 4 do
             let ys' = map (* i) ys
             let s' = s + i64.sum ys'
             in (ys', s')
         in d.1)
      xss
