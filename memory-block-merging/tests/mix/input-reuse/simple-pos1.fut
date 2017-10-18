-- Similar to reuse/input-reuse/simple-pos0.fut, but with two dimensions.
-- Requires coalescing for the input and output memory blocks to have the same
-- size.
-- ==
-- input { [[2, 5, 9], [1, 2, 3]] }
-- output { [[3, 6, 10], [2, 3, 4]] }
-- structure gpu { Alloc 0 }
-- structure cpu { Alloc 0 }

let main (xss: *[][]i32): [][]i32 =
  map (\xs -> map (+ 1) xs) xss
