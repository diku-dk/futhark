-- A concat of a split should be removed, evne across an inner
-- dimension.
-- ==
-- input { [[1,2],[3,4]] } output { [[1,2],[3,4]] }
-- structure { Split 0 Concat 0 }

let main(xs: [][]i32) =
  let (a,b) = split@1 1 xs
  in concat@1 a b
