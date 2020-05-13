-- ==
-- random input { 10 [20]i32 } auto output
-- structure cpu { Update 1 }

let main (n: i32) (xs: *[]i32) =
  unsafe
  xs with [0:n] = map (+2) (iota n)
