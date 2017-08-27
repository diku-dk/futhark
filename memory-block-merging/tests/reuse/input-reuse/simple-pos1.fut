-- Similar to simple-pos0.fut, but with two dimensions.
-- ==
-- input { [[2, 5, 9], [1, 2, 3]] }
-- output { [[3, 6, 10], [2, 3, 4]] }
-- structure gpu { Alloc 0 }

-- FIXME: Make loops over more than one dimension work.
-- structure cpu { Alloc 0 }

let main (xss: *[][]i32): [][]i32 =
  map (\xs -> map (+ 1) xs) xss
