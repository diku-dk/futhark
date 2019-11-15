-- Mapping a concatenation is turned into a single concat.
-- ==
-- input { [[1,2],[4,5],[7,8]] [[3],[6],[9]] [[3],[6],[9]] }
-- output { [[1,2,3,3],[4,5,6,6],[7,8,9,9]] }
--
-- input { [[1,2],[4,5],[7,8]] [[3,2,1],[6,5,4],[9,8,7]] [[0],[3],[6]] }
-- output { [[1,2,3,2,1,0],[4,5,6,5,4,3],[7,8,9,8,7,6]] }
-- structure { Map 0 Concat 1 }

let main [a][b][c] (xss: [][a]i32) (yss: [][b]i32) (zss: [][c]i32) =
  let n = a + b + c in
  map3 (\xs ys zs -> xs ++ ys ++ zs :> [n]i32) xss yss zss
