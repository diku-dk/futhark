-- ==
-- input { [0i64, 1i64]
--         [[[1i32,2i32,3i32],[4i32,5i32,6i32]],
--          [[7i32,8i32,9i32],[10i32,11i32,12i32]]] }
-- auto output

def main [k] [n] [m] (is: [k]i64) (ass: [k][n][m]i32) =
  map2 (\i a ->
          let x = map (\row -> opaque row[i:]) a
          let y = map (\row -> opaque row[:i]) a
          in (map2 concat x y) :> [n][m]i32)
       is
       ass
