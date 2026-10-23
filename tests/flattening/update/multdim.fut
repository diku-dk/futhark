-- ==
-- input { [0i64,1i64]
--         [2i64,3i64]
--         [[[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32]],
--          [[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32]]]
--         [[[0f32,1f32],[2f32,3f32]],[[4f32,5f32],[6f32,7f32]]]
--       }
-- output { [78.0f32, 94.0f32] }
-- structure gpu { /WithAcc 1 Update 0 }

def main [n] (is: [n]i64) (js: [n]i64) (ass: [n][][]f32) (vss: [n][][]f32) =
  #[flattening(only_inner)]
  map4 (\i j as vs -> f32.sum (flatten (copy as with [i:j, i:j] = vs))) is js ass vss
