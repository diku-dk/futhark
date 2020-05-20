-- ==
-- random input { [10][20][2]i32 } auto output
-- structure cpu { Update 1 }
-- structure gpu { Update 0 }

let main [n] (xs: *[n][][]i32) =
  unsafe
  xs with [:,2,1] = map (+2) (iota n)
