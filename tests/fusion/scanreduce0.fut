-- Horizontal fusion of scan and reduce.
-- ==
-- input { [1,2,3,4] } output { [1, 3, 6, 10] 24 }
-- structure { Screma 1 }
-- structure distributed { Kernel 4 }

let main (xs: []i32) =
  (scan (+) 0 xs, reduce (*) 1 xs)
