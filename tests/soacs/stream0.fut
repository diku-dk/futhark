-- Stream with an embedded map.  The OpenCL code generator messed this
-- up once.
--
-- ==
-- input { 10i64 1 1 }
-- output { [[0], [1], [1], [1], [1], [1], [1], [1], [1], [1]] }

def main(num_mc_it: i64)
        (num_dates: i32)
        (num_und: i32): [][]i32 =
  let sobvctsz  = num_dates*num_und in
  map_stream (\chunk (ns: [chunk]i32): [chunk][1]i32 ->
               map (\k: [1]i32 -> if ns[k]==0 then [0] else [1])
                   (iota chunk))
            (map i32.i64 (iota num_mc_it))
