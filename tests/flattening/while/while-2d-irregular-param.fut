-- ==
-- input { [[3i64, 1i64, 4i64], [2i64, 5i64, 3i64]] }
-- auto output
def main [n] [m] (xss: [n][m]i64) =
  map (\xs ->
         map (\x ->
                let zs = iota x
                let (_, res) =
                  loop (i, arr) = (0i64, zs)
                  while i < x do
                    let arr' = opaque (map (\z -> z * i + x) arr)
                    in (i + 1, arr')
                in reduce (+) 0 res + x)
             xs)
      xss
