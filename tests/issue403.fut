-- ==
-- input { [1,2] 0 } output { true }
-- input { [1,2] 1 } output { false }

let main (xs: *[]i32) (i: i32) =
  let xs[0] = 0
  in xs[i] == 0
