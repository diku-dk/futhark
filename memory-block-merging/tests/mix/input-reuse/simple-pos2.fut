-- Similar to simple-pos1.fut, but with three dimensions.
-- ==
-- input { [[[2, 5, 9], [1, 2, 3]]] }
-- output { [[[3, 6, 10], [2, 3, 4]]] }
-- structure gpu { Alloc 0 }
-- structure cpu { Alloc 0 }

let main (xsss: *[][][]i32): [][][]i32 =
  map (\xss -> map (\xs -> map (+ 1) xs) xss) xsss
