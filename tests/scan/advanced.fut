-- ==
-- entry: main
--
-- compiled random input { [10000000]f32 } auto output

let main [n] (as: [n]i32): ([n]i32, [n]i32, [n]i32, [n]i32, [n]i32) =
  let (bs, cs) = unzip <| map (\a -> (a - 1, a + 1)) as
  let (ds, es) = unzip <| scan (\(xb, xc) (yb, yc) -> (xb + yb, xc + yc)) (0, 0) <| zip bs cs
  in (as, bs, cs, ds, es)
