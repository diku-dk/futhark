-- A concat of a partition should be removed, too.
-- ==
-- input { [1,2,3] } output { [1,2,3] }
-- structure { Split 0 Concat 0 }

let main(xs: []i32) =
  let (a,b) = partition ((<2)) xs
  in concat a b
