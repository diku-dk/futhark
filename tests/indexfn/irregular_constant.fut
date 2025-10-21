-- map (\i -> map (\j -> i) (iota i)) (iota n)  
def irregular_constant (n: {i64 | \x -> Range x (0,inf)}) : {[]i64 | \_ -> true} =
  let sizes = iota n
  let offsets = scan (+) 0 sizes
  let total = offsets[n-1]
  let flags = scatter (replicate total 0) offsets (replicate n 1)
  in scan (+) 0 flags
