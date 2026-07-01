-- ==
-- input { [[1i64, 2i64, 3i64]] }
-- auto output

def main [n] [m] (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
         let (row_sum, scaled_prod) =
           ( reduce (+) 0 xs
           , reduce (*) 1 (map (* 5) xs)
           )
         in loop acc = row_sum
            for i < 3 do
              acc + i * 2 + scaled_prod)
      xss
