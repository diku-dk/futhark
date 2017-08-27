-- The output memory of the inner body can reuse the input memory.  Very basic.
-- ==
-- input { [2, 5, 9] }
-- output { [3, 6, 10] }
-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

let main (xs: *[]i32): []i32 =
  map (+ 1) xs
