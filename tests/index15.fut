-- Indexing the result of a map whose element is an array, where the map input
-- is a transposed reshape. The simplifier must not try to index the transpose
-- with too few indices.
-- ==
-- input { 2i64 3i64 [0,1,2,3,4,5] 2i64 }
-- output { 2 }

def main (n: i64) (m: i64) (xs: []i32) (i: i64) =
  let zs = map (\ys -> ys[0]) (transpose (unflatten (xs :> [n*m]i32)))
  in zs[i]
