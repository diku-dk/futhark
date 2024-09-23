def f [n] 't (conds: [n]bool) : i64 =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let indsT = scan (+) 0 tflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  in lst
