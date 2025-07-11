def reverse [n] (infty: i64) (xs : {[n]i64 | \x -> Range x (0,infty)}): {[n]i64 | \y -> Range y (0,infty)} =
  let iot = iota n
  let target_indices = map (\i -> n - 1 - i) iot
  let zeros = replicate n 0
  let y = scatter zeros target_indices xs
  in y
