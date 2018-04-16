-- Fusion of concats.
-- ==
-- input { [1] [2] [3] } output { [1,2,3] }
-- structure { Concat 1 }

let main (xs: []i32) (ys: []i32) (zs: []i32) =
  concat xs (concat ys zs)
