-- ==
-- input { [[1i64,2i64,3i64]] }
-- auto output

def main [n] [m] (xss: [n][m]i64) =
  map (\xs ->
         let (a, b) =
           ( reduce (+) 0 xs
           , reduce (*) 1 (map (* 5) xs)
           )
         let r = loop s = a for i < 3 do s + i * 2 + b
         in r)
      xss
