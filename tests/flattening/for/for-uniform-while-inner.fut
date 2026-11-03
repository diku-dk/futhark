-- ==
-- input { [[5i64, 3i64, 10i64], [2i64, 4i64, 6i64]] }
-- auto output
-- structure gpu { /Loop/ForLoop 1 /Loop/Loop/WhileLoop 1 }

def main [n] [m] (xss: [n][m]i64) =
  #[flattening(only_inner)]
  map (\xs ->
         loop acc = xs
         for i < 5 do
           loop acc2 = acc
           while reduce (+) 0 acc2 > 0 do
             map (\x -> x - 1) acc2)
      xss
