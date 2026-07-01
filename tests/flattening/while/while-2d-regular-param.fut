-- ==
-- input { [[3i64, 1i64, 4i64], [2i64, 5i64, 3i64]] }
-- auto output
def main [n] [m] (xss: [n][m]i64) =
  map (\xs ->
         map (\x ->
                let extra0 = map (\z -> z + x) xs
                let (_, arr, extra) =
                  loop (i, arr, extra) = (0i64, xs, extra0)
                  while i < x do
                    let arr' = opaque (map (\z -> z * i + x) arr)
                    let extra' = opaque (map (\z -> z + i) extra)
                    in (i + 1, arr', extra')
                in reduce (+) 0 arr + reduce (+) 0 extra + x)
             xs)
      xss
