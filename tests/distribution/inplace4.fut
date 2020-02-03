-- Distributing an in-place update of slice with a bounds check.
-- ==
-- tags { no_opencl }
-- input { [[1,2,3],[4,5,6]] [0,1] [42,1337] }
-- output { [[42,1337,3],[4,42,1337]] }
-- structure distributed { SegMap/Update 0 }

let main [n][m] (xss: *[n][m]i32) (is: [n]i32) (ys: [2]i32) =
  map2 (\xs i -> copy xs with [i:i+2] = ys) xss is
