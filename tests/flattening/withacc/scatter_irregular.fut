-- Maximally irregular case.
-- ==
-- input { [0i64, 2, 1, 3, 2, 3, 10]
--         [2,3,1,4,5,6,7]
--         [1i64, 2, 3, 4]
--         [4i64, 3, 4, 5]
--       }
-- output { [2, 1, 3, 4] }

def main is vs =
  map2 \n m ->
    let arr = scatter (replicate n 0i32) (take m is) (take m vs)
    in last (opaque arr)
