-- Slicing a replicate should work.
--
-- ==
-- input { 3i64 [1,2] } output { [[1,2],[1,2]] }

def main [b] (m: i64) (diag: [b]i32) : [][]i32 =
  let top_per = replicate m diag
  in top_per[1:m]
