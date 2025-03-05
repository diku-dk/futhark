-- Based on #2226. A segmented reduction (with a somewhat contrived
-- operator), where the operands are arrays.
--
-- ==
-- input { [[1.0,2.0],[2.0,3.0],[4.0,5.0]] }

entry main [n] (input: [n][2]f64) : [2][2]f64 =
  map (\i ->
         let t = map2 (\_ b -> [b[0], 0]) [0, 0] (input[i:i + 2] :> [2][2]f64)
         in reduce (\_ _ -> [0, 0]) [0, 0] t)
      [0, 1]
