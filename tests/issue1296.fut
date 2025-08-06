-- ==
-- structure { Concat 0 Replicate 1 }

def main (n: i64) =
  let xs = replicate n 0
  let ys = replicate n 1
  let xs' = map (+ 1) xs
  let zs = concat xs' ys
  in zs
