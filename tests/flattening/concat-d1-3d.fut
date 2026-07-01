-- ==
-- input { [0i64, 1i64]
--         [[[[1i32,2i32],[0i32,4i32],[5i32,6i32]],
--           [[7i32,8i32],[9i32,10i32],[11i32,12i32]]],
--          [[[13i32,14i32],[1i32,4i32],[17i32,18i32]],
--           [[19i32,20i32],[100i32,5i32],[23i32,24i32]]]] }
-- auto output

let main [k][a][b][c] (is: [k]i64) (xsss: [k][a][b][c]i32) =
  map2
    (\i x ->
       let p = map (\rows -> opaque rows[i:]) x
       let q = map (\rows -> opaque rows[:i]) x
       in (map2 concat p q) :> [a][b][c]i32)
    is
    xsss
