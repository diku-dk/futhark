-- Minimal test for fusing map-flatten-scatter, shrunk from ker2-radix.fut.
-- The map produces two arrays which are flattened and fed into a scatter.
-- The outer map should be fused into the scatter's WithAcc (map-flatten-scatter fusion),
-- leaving no top-level Screma.
-- ==
-- entry: main
-- input { [[1i32,2i32],[3i32,4i32]] [0i32,0i32,0i32,0i32] }
-- output { [2i32,3i32,4i32,5i32] }

-- f maps each row: returns (vals = row+1, inds = iota offset by row index)
def f [n] (i: i64) (x: [n]i32) : ([n]i32, [n]i64) =
  (map (+1) x, map (+ (i * n)) (iota n))

entry main [m][n] (xs: [m][n]i32) (dest: *[m*n]i32) : *[m*n]i32 =
  let (vals, inds) = unzip (map2 f (iota m) xs)
  in scatter dest (flatten inds) (flatten vals)
