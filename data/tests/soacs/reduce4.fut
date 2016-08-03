-- Reduction where the accumulator is an array.
-- ==
-- input { [[1,2],[3,4],[5,6]] }
-- output { [9, 12] }
fun []int main([][m]int as) =
  reduceComm(fn []int ([]int acc, []int r) =>
               zipWith(+, acc, r),
             replicate(m, 0),
             as)
