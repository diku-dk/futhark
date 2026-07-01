-- For loop nested in map with statements before and after the loop.
-- ==
-- input { [1i64, 2i64, 3i64] }
-- auto output

def main [n] (xs : [n]i64) =
  map (\x ->
         let y = x * 2
         let z = loop acc = y for i < 4 do
                   acc + i
         in z * x
      ) xs
