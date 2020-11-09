-- Nested concatenation with more arrays.
-- ==
-- input { [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] }
-- output { [[1,2,1,2,1,2], [3,4,3,4,3,4], [5,6,5,6,5,6]] }
-- structure distributed { Kernel 0 }

let main [a][b][c] (xss: [][a]i32) (yss: [][b]i32) (zss: [][c]i32) =
  let n = a + b + c in
  map3 (\xs ys zs -> concat xs (concat ys zs) :> [n]i32) xss yss zss
