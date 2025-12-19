def scatter_perm (n : {i64 | \n -> Range n (0,inf)}): {[n]i64 | \y -> Range y (0,n-1)} =
  let iot = iota n
  let target_indices = map (\i -> n - 1 - i) iot
  let zeros = replicate n 0
  let y = scatter zeros target_indices iot
  in y
-- def scatter_perm (n : {i64 | (>= 0)}): {[n]i64 | \y -> Range y (1,n)} =
--   let ones = replicate n 1
--   let acc = scan (+) 0 ones
--   let target_indices = map (\i -> n-i) acc
--   let zeros = replicate n 0
--   let y = scatter zeros target_indices acc
--   in y
