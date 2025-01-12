-- ==
-- input { [0i64,1i64]
--         [2i64,3i64]
--         [[[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64]],
--          [[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64],[9f64,9f64,9f64,9f64]]]
--         [[[0f64,1f64],[2f64,3f64]],[[4f64,5f64],[6f64,7f64]]]
--       }
-- output { [78.0, 94.0] }

let main [n] (is : [n]i64) (js : [n]i64) (ass : [n][][]f64) (vss : [n][][]f64) =
  map4(\i j as vs -> f64.sum(flatten(copy as with [i:j,i:j] = vs))) is js ass vss
