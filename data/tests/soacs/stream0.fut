-- Stream with an embedded map.  The OpenCL code generator messed this
-- up once.
--
-- ==
-- input { 10 1 1 }
-- output { [[0], [1], [1], [1], [1], [1], [1], [1], [1], [1]] }

fun [[int]] main(int num_mc_it,
                  int num_dates,
                  int num_und) =
  let sobvctsz  = num_dates*num_und in
  streamMapMax(fn [[int,1]] (int chunk, [int] ns) =>
                 map( fn [int,1] (int k) =>
                        if k==0 then [0] else [1]
                    , iota(chunk) )
              , iota(num_mc_it))
