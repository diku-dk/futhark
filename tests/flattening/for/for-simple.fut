-- ==
-- input { [1i64, 5i64, 10i64] }
-- auto output

def main [n] (xs: [n]i64) =
  map (\x ->
         let y = x * 2
         let z =
           loop acc = y
           for i < 4 do
             acc + i
         in z * x)
      xs
