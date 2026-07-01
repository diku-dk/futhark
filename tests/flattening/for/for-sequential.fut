-- ==
-- input { [0i64, 1i64, 5i64, 10i64] }
-- auto output

def main [n] (xs: [n]i64) =
  map (\x ->
         let acc =
           loop a = x
           for i < 10 do
             a + i
         let mid = acc * 2
         in loop b = mid
            for j < mid do
              b + j)
      xs
