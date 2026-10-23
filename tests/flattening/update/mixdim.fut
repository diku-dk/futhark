-- Mixing slices and indexes in complex ways.
-- ==
-- input { [0i64,1i64]
--         [2i64,3i64]
--         [[[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32]],
--          [[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32],[9f32,9f32,9f32,9f32]]]
--         [[0f32,1f32],[4f32,5f32]]
--       }
-- output { [91.0f32, 99.0f32] }
-- structure gpu { /WithAcc 1 SegScan 0 Apply 0 }

def main [n] (is: [n]i64) (js: [n]i64) (ass: [n][][]f32) (vs: [n][]f32) =
  #[flattening(only_inner)]
  map4 (\i j as vs -> f32.sum (flatten (copy as with [i, i:j] = vs))) is js ass vs
