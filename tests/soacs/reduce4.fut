-- Reduction where the accumulator is an array.
-- ==
-- input { [[1,2],[3,4],[5,6]] }
-- output { [9, 12] }
let main [m] (as: [][m]i32): []i32 =
  reduce_comm(\(acc: []i32) (r: []i32): []i32  ->
               map2 (+) acc r)
             (replicate m 0)
             as
