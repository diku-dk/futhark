-- Stream with an embedded map.  The OpenCL code generator messed this
-- up once.
--
-- ==
-- input { 10 1 1 }
-- output { [[0], [1], [1], [1], [1], [1], [1], [1], [1], [1]] }

fun main(num_mc_it: i32,
         num_dates: i32,
         num_und: i32): [][]i32 =
  let sobvctsz  = num_dates*num_und in
  stream_map (\(ns: [chunk]i32): [chunk][1]i32 ->
               map (\(k: i32): [1]i32 -> if k==0 then [0] else [1])
                   (iota chunk))
            (iota num_mc_it)
