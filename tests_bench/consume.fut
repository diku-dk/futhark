-- ==
-- input { [1,2,3] }
-- output { [1,3,3] }

let main (xs: *[]i32) =
  xs with [1] = xs[1] + 1
