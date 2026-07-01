-- ==
-- input { [1i64, 0i64]
--         [[[[1i64,2i64],[3i64,4i64]],[[5i64,6i64],[7i64,8i64]]],
--          [[[9i64,10i64],[11i64,12i64]],[[13i64,14i64],[15i64,16i64]]]] }
-- auto output

def main [k] [a] [b] [c] (is: [k]i64) (xsss: [k][a][b][c]i64) =
  map2 (\i x ->
          let p = map (\rows -> map (\row -> opaque row[i:]) rows) x
          let q = map (\rows -> map (\row -> opaque row[:i]) rows) x
          in (map2 (map2 concat) p q) :> [a][b][c]i64)
       is
       xsss
