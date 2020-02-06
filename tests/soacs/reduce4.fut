-- Reduction where the accumulator is an array.
-- ==
-- input { [[1,2],[3,4],[5,6]] }
-- output { [9, 12] }

let main [n][m] (as: [n][m]i32): []i32 =
  reduce_comm (map2 (+)) (replicate m 0) as
