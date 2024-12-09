-- Mixing slices and indexes in complex ways.
-- ==
-- input { [0i64,1i64]
--         [2i64,3i64]
--         [[[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64]],
--          [[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64]]]
--         [[0f64,1f64],[4f64,5f64]]
--       }
-- output { [91.0, 99.0] }

let main [n] (is : [n]i64) (js : [n]i64) (ass : [n][][]f64) (vs : [n][]f64) =
  map4(\i j as vs -> f64.sum(flatten(copy as with [i,i:j] = vs))) is js ass vs
