def f [n] 't (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
  let y = map (\x -> x) xs
  in y
