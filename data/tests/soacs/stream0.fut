-- Stream with an embedded map.  The OpenCL code generator messed this
-- up once.
--
-- ==
-- input { 10 1 1 }
-- output { [[0], [1], [1], [1], [1], [1], [1], [1], [1], [1]] }

fun main(num_mc_it: int,
                  num_dates: int,
                  num_und: int): [][]int =
  let sobvctsz  = num_dates*num_und in
  streamMap (fn (chunk: int) (ns: []int): [][1]int  =>
              map (fn (k: int): [1]int  =>
                     if k==0 then [0] else [1]
                 ) (iota(chunk) )
           ) (iota(num_mc_it))
