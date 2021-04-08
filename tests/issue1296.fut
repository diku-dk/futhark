-- ==
-- structure { Concat 0 Copy 0 }

let main(n: i64) =
  let xs = replicate n 0
  let ys = replicate n 1
  let xs' = map (+ 1) xs
  let zs = concat xs' ys
  in zs
