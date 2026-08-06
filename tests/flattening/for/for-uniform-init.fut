-- ==
-- input { [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] }
-- auto output
-- structure gpu { /Loop 1 /Loop/ForLoop 1 }

def main [n] [m] (xss: [n][m]i64) =
  #[incremental_flattening(only_inner)]
  map (\xs ->
         let ys = map (* 2) xs
         let res =
           loop zs = ys
           for i < 4 do
             map (+ i) zs
         in res)
      xss
