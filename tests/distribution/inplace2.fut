-- Ensure that in-place updates with variant indexes/values are
-- distributed sensibly.
-- ==
-- input { [[1,2], [3,4]] [0,1] [42,1337] }
-- output { [[42,2], [3,1337]] }

let main (xss: *[][]i32) (is: []i32) (vs: []i32) =
  map3 (\(xs: []i32) i v -> unsafe copy xs with [i] <- v) xss is vs
