-- when distributing, the stream should be removed and the body
-- distributed.
--
-- ==
-- structure distributed { MapKernel 2 Map 0 Reduce 0 }

fun []int main([][n]int a) =
  map(fn int ([]int a_row) =>
        streamSeq( fn int (int chunk, int acc, []int c) =>
                     let w = filter( >6, c ) in
                     let w_sum = reduce(+, 0, w) in
                     acc+w_sum
                 , 0, a_row
                 ),
        a)
