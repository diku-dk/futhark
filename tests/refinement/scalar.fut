def f [n] 't (x: i64) : {[x]i64 | \res-> permutationOf res (0...n-1)} =
  let y = replicate x x
  in y
