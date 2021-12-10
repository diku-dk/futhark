def main n b =
  let xs = replicate n 0
  let xs' = if b then loop xs = copy xs for i < 10 do xs with [i] = 0 else xs
  in if b then xs else xs'
