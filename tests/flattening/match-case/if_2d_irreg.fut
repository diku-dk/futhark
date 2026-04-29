-- ==
-- input { [[5i64, 10i64, 15i64], [3i64, 4i64, 15i64]] [1i64, 4i64] }
-- auto output
def main [n] [m] (xss: [n][m]i64) (ys: [n]i64) =
  map (\xs ->
         map (\x ->
                opaque (if x >= 10 then map (* x) ys else map (+ x) ys))
             xs)
      xss
