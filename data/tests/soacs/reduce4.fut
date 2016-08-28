-- Reduction where the accumulator is an array.
-- ==
-- input { [[1,2],[3,4],[5,6]] }
-- output { [9, 12] }
fun main(as: [][m]int): []int =
  reduceComm(fn (acc: []int) (r: []int): []int  =>
               zipWith((+), acc, r),
             replicate(m, 0),
             as)
