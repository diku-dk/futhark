def f [n] 't (conds: [n]bool) : {[n]i64 | \_ -> true} =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let tmp   = scan (+) 0 fflgs
  in tmp
