-- ==
-- input { [[0,0,0], [0,0,0]] }
-- output { [[2,3,4], [0,0,0]] }
-- structure cpu { Update 1 }
-- structure gpu { Update 0 }

let main [n] (xs: *[][n]i32) =
  unsafe
  xs with [0] = map (+2) (iota n)
