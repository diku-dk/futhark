-- A concat of a split should be removed.
-- ==
-- input { [1,2,3] } output { [1,2,3] }
-- structure { Split 0 Concat 0 }

let main(xs: []i32) =
  let (a,b) = split 1 xs
  in concat a b
