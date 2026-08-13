-- ==
-- input { [[5i64, 10i64, 15i64], [3i64, 4i64, 15i64]] [[1i64, 4i64], [2i64, 3i64]] }
-- auto output
def main [n] [m] [k] (xss: [n][m]i64) (yss: [n][k]i64) =
  map (\xs ->
         map (\x ->
                opaque (if x >= 10 then map (map (* x)) yss else map (map (+ x)) yss))
             xs)
      xss
