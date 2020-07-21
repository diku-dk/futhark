-- Testing big arrays.
-- ==
-- no_opencl compiled input { 2 1100000000 1 1073741823 } output { 255u8 }
-- no_opencl compiled input { 3 1073741824 2 1073741823 } output { 255u8 }
-- structure { Replicate 1 Iota 1 }

let main (n: i32) (m: i32) (i: i32) (j: i32) =
  -- The opaque is just to force manifestation.
  (opaque (replicate n (tabulate m (\i -> u8.i32 i))))[i,j]
