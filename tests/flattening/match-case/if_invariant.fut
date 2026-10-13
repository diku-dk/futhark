-- ==
-- input { [5i64, 10i64, 15i64] [1i64, 4i64, 3i64] 5i64 }
-- auto output
-- input { [5i64, 10i64, 15i64] [1i64, 4i64, 3i64] 6i64 }
-- auto output
def main [n] (xs: [n]i64) (ys: [n]i64) (b: i64) =
  map (\x ->
         opaque (if b == 5 then map (* x) ys else map (+ x) ys))
      xs
