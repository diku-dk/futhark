-- Based on #2227. A segmented reduction (with a somewhat contrived
-- operator), where the operands are arrays.
--
-- ==
-- input { [[1.0f32,2.0f32],[2.0f32,3.0f32],[4.0f32,5.0f32]] }
-- structure gpu { /SegRed 1 }

entry main [n] (input: [n][2]f32) : [2][2]f32 =
  #[flattening(only_inner)]
  map (\i ->
         let t = map2 (\_ b -> [b[0], 0]) [0, 0] (input[i:i + 2] :> [2][2]f32)
         in reduce (\_ _ -> [0, 0]) [0, 0] t)
      [0, 1]
