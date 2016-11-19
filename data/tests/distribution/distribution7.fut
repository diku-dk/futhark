-- when distributing, the stream should be removed and the body
-- distributed.
--
-- ==
-- tags { no_opencl }
-- structure distributed { Kernel 5 }

fun main(a: [][n]int): []int =
  map (fn (a_row: []int): int  =>
        streamSeq (fn (chunk: int) (acc: int) (c: []int): int  =>
                     let w = filter (>6) c
                     let w_sum = reduce (+) 0 w in
                     acc+w_sum
                 ) 0 (a_row
                 )) a
