-- Reduction where the accumulator is an array.
-- ==
-- input { [[1,2],[3,4],[5,6]] }
-- output { [9, 12] }
fun main(as: [][m]i32): []i32 =
  reduceComm(\(acc: []i32) (r: []i32): []i32  ->
               map (+) acc r)
             (replicate m 0)
             as
