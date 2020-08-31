-- Testing big arrays.
-- ==
-- tags { no_python }
-- no_python no_opencl compiled input { 2 1100000000 1 1073741823 } output { -2i8 }
-- no_python no_opencl compiled input { 3 1073741824 2 1073741823 } output { -3i8 }
-- structure gpu { SegMap 1  }

let main (n: i32) (m: i32) (i: i32) (j: i32) =
  -- The opaque is just to force manifestation.
  (opaque (tabulate_2d n m (\i j -> i8.i32 (i ^ j))))[i,j]
