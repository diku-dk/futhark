-- when distributing, the stream should be removed and the body
-- distributed.
--
-- ==
-- tags { no_opencl }
-- structure distributed { Kernel 5 }

fun main(a: [][n]i32): []i32 =
  map (\(a_row: []i32): i32  ->
        streamSeq (\(acc: i32) (c: [chunk]i32): i32  ->
                     let w = filter (>6) c
                     let w_sum = reduce (+) 0 w in
                     acc+w_sum
                 ) 0 (a_row
                 )) a
