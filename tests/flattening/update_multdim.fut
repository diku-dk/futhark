-- TODO: add test case(s) once this compiles.
-- ==
-- tags { disable }

let main [n] (is : [n]i64) (js : [n]i64) (ass : [n][][]f64) (vss : [n][][]f64) =
  map4(\i j as vs -> (copy as with [i:j,i:j] = vs)) is js ass vss
