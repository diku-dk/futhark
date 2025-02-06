def f [n] 't (conds: [n]bool) : {[n]i64 | \_ -> true} =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let indsT = scan (+) 0 tflgs
  in indsT
