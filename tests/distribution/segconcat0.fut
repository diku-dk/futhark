-- Nested concatenation just becomes a concatenation along an inner dimension.
-- ==
-- input { [[1,2,3],[4,5,6]] [[3,2,1],[6,5,4]] }
-- output { [[1,2,3,3,2,1],
--           [4,5,6,6,5,4]] }
-- structure distributed { Kernel 0 }

let main [n][m] (xss: [][n]i32) (yss: [][m]i32) =
  let k = n + m in
  map (\(xs, ys) -> concat xs ys :> [k]i32) (zip xss yss)
